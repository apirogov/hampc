//TODO:
//Browse database, add stuff
//add Settings panel:
//  add output state+toggling
//  MPD password setting -> Cookie?
//add Query support
//add saved playlist support
//include stream?
//FIXME:
//reduce glitches
//fix problem with hanging idle connections - proper timeout
//make usable on phone - jqueryui sortable + mobile + touch-punch?

//latest infos from server
var MPD = {
  status : null,
  queue  : null,
  current: null,
  outputs: null,
  waiting: 0 //number of running requests
}
var sliderCooldown = false; //lock to prevent sliders to fire too much

//entry function
$(document).ready(function(){
  //init sliders
  $("#volume").slider();
  $("#progress").slider({tooltip:'hide',id:"progressbar"}); //id required for css
  
  //init local timer (current pos, length)
  trackTimer();

  //initial interface update
  updateInterface();

  //event handlers
  $('#navqueue').click(loadTabCallback('#navqueue','#panelQueue'));
  $('#navbrowse').click(loadTabCallback('#navbrowse','#panelBrowse'));
  $('#navsettings').click(loadTabCallback('#navsettings','#panelSettings'));

  $('#btnprevious').click(onPreviousClick);
  $('#btnstop').click(onStopClick);
  $('#btnpause').click(onPauseClick);
  $('#btnnext').click(onNextClick);
  $('#volume').on('slideStop', onVolumeSlide);

  $('#progress').on('slideStop', onSongSlide);
  $('#queue').sortable({
    containerSelector: 'table',
    itemPath: ' > tbody ',
    itemSelector: 'tr',
    onDrag: onQueueDrag,
    onDrop: onQueueDrop
  });

  $('#btnconsume').click(onConsumeClick);
  $('#btnrepeat').click(onRepeatClick);
  $('#btnsingle').click(onSingleClick);
  $('#btnrandom').click(onRandomClick);
  $('#btnupdate').click(onUpdateClick);
  $('#btnclear').click(onClearClick);

  doPoll();          //init long polling
});

//update everything
function updateInterface() {
  updateQueue();
  updateControls();
  updateCurrentSong();
  updateOutputs();
}

//we dont ask MPD for the current position all the time
//if its playing, we just increment our local timer until paused/stopped
function trackTimer() {
  if (typeof trackTimer.enabled == "undefined") { //init
    trackTimer.time = [0,0];
    trackTimer.enabled = false;
  }

  if (trackTimer.enabled===true) {
    updateProgress(trackTimer.time);
    if (trackTimer.time[0]<trackTimer.time[1]) {
      trackTimer.time[0] = trackTimer.time[0]+1;
    } else {
      trackTimer.enabled = false;
    }
  }
  setTimeout(trackTimer, 1000);
}
// --- helper methods ---

function setClass(id, name, val) {
  if (val === true) {
    $(id).addClass(name);
  } else {
    $(id).removeClass(name);
  }
}
function setBtnActive(id, val) { setClass(id, 'active', val); }
function setBtnEnabled(id, val) { setClass(id, 'disabled', !val); }

function updatePauseButton(val) {
    if (val) {
      $("#btnpause-icon").removeClass("glyphicon-pause").addClass("glyphicon-play");
    } else {
      $("#btnpause-icon").removeClass("glyphicon-play").addClass("glyphicon-pause");
    }
}

function updateStateIcon(state) {
  if (state=="Playing") {
    $("#state").removeClass().addClass("glyphicon glyphicon-play");
  } else if (state=="Paused") {
    $("#state").removeClass().addClass("glyphicon glyphicon-pause");
  } else if (state=="Stopped") {
    $("#state").removeClass().addClass("glyphicon glyphicon-stop");
  } else {
    $("#state").removeClass().addClass("glyphicon glyphicon-remove-circle");
  }
}

// Int -> String (number to MM:SS)
function secToTime(t) {
    var sec_num = parseInt(t, 10); // don't forget the second param
    var minutes = Math.floor(sec_num  / 60);
    var seconds = sec_num - (minutes * 60);
    if (minutes < 10) {minutes = "0"+minutes;}
    if (seconds < 10) {seconds = "0"+seconds;}
    return minutes+':'+seconds;
}

// Update progress bar and time label
function updateProgress(time) {
  if (time===null) {
    $("#progress").slider({min: 0, max: 0, value: 0});
    $("#time").text("");
  } else {
    $("#progress").slider({min: 0, max: time[1]}).data('slider').setValue(time[0]);
    $("#time").text(secToTime(time[0])+" / "+secToTime(time[1]));
  }
}

// Highlight current song in bold (waits until all get requests finish except poll)
function updateQueueHighlighted(index) {
    if (index==null || MPD.status.stState=="Stopped")
      return;

    if (MPD.waiting != 0) { //not sync -> wait and retry
      setTimeout(function(){updateQueueHighlighted(index);}, 200);
      return;
    }

    $('#queue > tbody').children('tr').removeClass().css('font-weight', 'normal');
    $('#queue > tbody').children('tr:nth-child('+(index+1)+')')
        .addClass('active').css('font-weight','bold');
}

// helper, either extracts tag value, or returns default
function getTag(tag, defval) {
  return (typeof tag === "undefined") ? defval||"" : tag[0];
}

// helper to insert a (clickable) glyphicon
function icon(str, click) {
  return '<span class="glyphicon glyphicon-'+str+'"'
    + (typeof click != "undefined" ? ' onclick="'+click+'" style="cursor:pointer"':'')
    +'></span>';
}

// helper to generate a row for the queue from a song
function rowFromSong(song) {
  var tags = song.sgTags;
  var artist = getTag(tags.Artist);
  var file = song.sgFilePath.replace(/^.*[\\\/]/, '');
  var title = getTag(tags.Title, file);
  var dur = secToTime(parseInt(song.sgLength, 10));
  var r = '<tr>'
  r += '<td>'+(song.sgIndex+1)+'</td>';
  r+='<td onclick="onQueuePlay('+song.sgIndex+')" style="cursor:pointer;">';
  if (artist != '')
    r += artist + ' - ';
  r += title + '</td><td>' + dur + '</td>';
  r += '<td>' +icon('trash','onQueueTrash('+song.sgIndex+');') +'</td>';
  r += '</tr>';
  return r;
}

// ---- update methods (read from MPD) ----

function updateCurrentSong(song) {
  mpdget('current', function(data) {
    MPD.current = JSON.parse(data);
    var song = MPD.current;

    if (song===null) {
      $("#artist").text("");
      $("#album").text("");
      $("#title").text("");
      $("#time").text("");
      $("#progress").data('slider').setValue(0);
      return;
    }
    var tags = song.sgTags;
    var artist = getTag(tags.Artist, '&nbsp;');
    var album = getTag(tags.Album, '&nbsp;');
    var file = song.sgFilePath.replace(/^.*[\\\/]/, '');
    var title = getTag(tags.Title, file);
    $("#artist").html(artist);
    $("#album").html(album);
    $("#title").html(title);

    updateQueueHighlighted(song.sgIndex);
  });
}

function updateControls() {
  mpdget('status', function(data) {
    MPD.status = JSON.parse(data);
    var stat = MPD.status;

    //update controls on top
    setBtnEnabled("#btnstop", stat.stState!="Stopped");
    setBtnEnabled("#btnnext", stat.stState!="Stopped");
    setBtnEnabled("#btnprevious", stat.stState!="Stopped");
    updatePauseButton(stat.stState!="Playing");
    $("#volume").slider().data('slider').setValue(stat.stVolume);

    //update button state in right column
    setBtnActive("#btnrandom", stat.stRandom);
    setBtnActive("#btnconsume", stat.stConsume);
    setBtnActive("#btnsingle", stat.stSingle);
    setBtnActive("#btnrepeat", stat.stRepeat);

    setBtnEnabled("#btnupdate", stat.stUpdatingDb==null)
    setBtnEnabled("#btnclear", stat.stPlaylistLength!=0)

    //update running timer
    trackTimer.enabled = stat.stState=="Playing"
    trackTimer.time = stat.stTime;
 
    updateStateIcon(stat.stState);
  });
}

function updateQueue() {
  mpdget('queue', function(data) {
      MPD.queue = JSON.parse(data); 
      var songs = MPD.queue;
      $('#queue > tbody').html('');
      $.each(songs, function(i, song) {
        $('#queue > tbody').append(rowFromSong(song));
      })
  });
}

function updateOutputs() {
  mpdget('outputs', function(data) {
      MPD.outputs = JSON.parse(data); 
      var outputs = MPD.outputs;
      $.each(outputs, function(i, output) {
        //TODO: Update state
      })
  });
}

//long-polling loop to hampd server
//Server says approximately what changed, the callback decides what requests are necessary
//there is 1 sec delay between polling requests
function doPoll() { setTimeout(poll, 1000); }
function poll() {
  $.get(withMPDpref('ping'), function() { //test connection to hampd server
      //if we had a connection problem, but now it is gone
      //-> update everything to prevent glitches
      if ($('#notify').is(':visible')) {
        $('#notify').text('').removeClass().addClass('alert').hide(); //clear error message
        updateInterface(); //get new data
      }
  });
  $.ajax({ //long poll
    url: withMPDpref('idle'),
    type: 'GET',
    async: true,
    cache: false,
    dataType: "json",
    timeout: 30000,
    complete: doPoll,
    error: function(jqXHR, text, error) {
      if (text!='timeout') { //connection problem!
        $('#notify').show().addClass('alert-danger').text('No connection to hampd!');
        trackTimer.enabled = false;
      }
    },
    success: function(data) {
      subsystems = JSON.parse(data);
      if (typeof subsystems.error != "undefined") { //hampd has no connection to MPD!
        $('#notify').show().addClass('alert-warning').text('No connection to MPD!');
        trackTimer.enabled = false;
        return;
      }
      //decide which calls to make
      $.each(subsystems, function(i, sub) {
        switch (sub) {
          case 'PlaylistS':
          case 'PlayerS':
            updateControls();
            updateQueue();
            updateCurrentSong();
            break;

          case 'UpdateS':
          case 'MixerS':
          case 'OptionsS':
            updateControls();
            break;

          case 'OutputS':
            updateOutputs();
            break;

          //we don't care about these (at the moment)
          case 'DatabaseS':
          case 'StoredPlaylistS':
          case 'StickerS':
          case 'SubscriptionS':
          case 'MessageS':
        }
      });
  }});
}

// -- helpers to communicate with hampd server --

//generate a full request url
function withMPDpref(str) { return 'mpd//'+str+'/'; }

//get request with semaphore-thingy for sync.
function mpdget(url, callback) {
  MPD.waiting++;
  $.ajax({
    url: withMPDpref(url),
    type: 'GET',
    async: true,
    cache: false,
    dataType: "json",
    error: function() { MPD.waiting--; },
    success: function(data) { callback(data); MPD.waiting--;}
  });
}

//get request where we dont care about results
function MPDexec(str) { $.get(withMPDpref(str), null); }

// ---- handling user input ----
function loadTabCallback(buttonid, panelid) {
  return function() {
    $('.panel-primary').hide();
    $('.navbar-nav').children('li').removeClass('active');
    $(buttonid).addClass('active');
    $(panelid).show();
  };
}

function onQueueTrash(index) {
  MPDexec('delete/'+index);
}
function onQueuePlay(index) {
  MPDexec('play/'+index);
}

var deb
function onQueueDrag(item, pos, sup, ev) {
  $('#queue').attr('data-previndex', item.index());
}
function onQueueDrop(item, pos, sup, ev) {
  var oldi = $('#queue').attr('data-previndex');
  var newi = item.index();
  if (oldi > newi) oldi--; //because index shift
  MPDexec('move/'+oldi+'/'+newi); //move item
  //cleanup
  $('#queue').removeAttr('data-previndex');
}

function onPreviousClick() { MPDexec('previous'); }
function onNextClick() { MPDexec('next'); }
function onStopClick() { MPDexec('stop'); }
function onPauseClick() {
  trackTimer.enabled ? MPDexec('pause/True') : MPDexec('play');
}

function onVolumeSlide() {
  MPDexec('volume/'+$('#volume').slider().data('slider').getValue());
}
function onSongSlide() {
  MPDexec('seek/'+$('#progress').slider().data('slider').getValue());
}

function btnToggler(id, url) {
  MPDexec(url+($(id).hasClass('active') ? '/False' : '/True'));
}
function onConsumeClick() { btnToggler('#btnconsume', 'consume'); }
function onRandomClick() { btnToggler('#btnrandom', 'random'); }
function onRepeatClick() { btnToggler('#btnrepeat', 'repeat'); }
function onSingleClick() { btnToggler('#btnsingle', 'single'); }
function onUpdateClick() { MPDexec('update'); }
function onClearClick() { MPDexec('clear'); }

