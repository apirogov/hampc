//latest infos from server and other stuff stored here
var MPD = {
  status : null,
  queue  : null,
  current: null,
  outputs: null,
  stream:  null,
  password: '',
  currdir: [], //currently browsed directory as path of indices
  lasttime: 0, //last timestamp received from server
  waiting: 0  //number of running requests
}

//read MPD pw if stored
if ($.localStorage.isSet('mpdpw')) {
  MPD.password = $.localStorage.get('mpdpw');
  $('#pw').val('blablablub'); //to keep away the too curious
}
//collect stream URL
var url = $('#streamurl').text().trim();
var media = null;
if (url != '') {
  MPD.stream = window.location.protocol+'//'+window.location.host+url
  media = {'mp3': MPD.stream}; //assume mp3
  if (MPD.stream.split('.').pop()=='ogg') //ogg -> oga type
    media = {'oga': MPD.stream};
}

//entry function
$(document).ready(function(){
  //init sliders
  $('#volume').slider();
  $('#progress').slider({tooltip:'hide',id:"progressbar"}); //id required for css

  //init local timer (current pos, length)
  trackTimer();
  //initial interface update
  updateInterface();

  //event handlers
  // nav
  $('#navqueue').click(loadTabCallback('#navqueue','#panelQueue'));
  $('#navbrowse').click(loadTabCallback('#navbrowse','#panelBrowse'));
  $('#navsettings').click(loadTabCallback('#navsettings','#panelSettings'));

  // url handler - simulate click
  var anchor = window.location.href.replace(/^.*#/, '');
  if (anchor=="settings") {
    $('#navsettings').trigger('click');
  } else if (anchor=="browse") {
    $('#navbrowse').trigger('click');
  }

  $('#btnprevious').click(onPreviousClick);
  $('#btnstop').click(onStopClick);
  $('#btnpause').click(onPauseClick);
  $('#btnnext').click(onNextClick);
  $('#volume').on('slideStop', onVolumeSlide);

  // queue
  $('#progress').on('slideStop', onSongSlide);

  new Sortable($('#queue > tbody')[0], {
    draggable: "tr",
    handle: ".sort-drag",
    onStart: onQueueDrag,
    onEnd: onQueueDrop
  });

  // right col
  $('#btnconsume').click(onConsumeClick);
  $('#btnrepeat').click(onRepeatClick);
  $('#btnsingle').click(onSingleClick);
  $('#btnrandom').click(onRandomClick);
  $('#btnupdate').click(onUpdateClick);
  $('#btnclear').click(onClearClick);

  // Settings
  $('#btnsetpw').click(onSetPwClick);
  $('#pw').keypress(onPwChange);

  //init jplayer if stream exists
  if (MPD.stream != null) {
    $('#stream-jplayer').jPlayer({
      ready: function() {
        $('#stream-jplayer').jPlayer('setMedia', media);
      },
      swfPath: 'js/Jplayer.swf',
    });
    $('#btnstream').click(onStreamClick);
  }

  doPoll();          //init long polling
});

//update everything
function updateInterface() {
  updateQueue();
  updateControls();
  updateCurrentSong();
  updateOutputs();
  updateDirectory();
}

//we dont ask MPD for the current position all the time
//if its playing, we just increment our local timer until paused/stopped
function trackTimer() {
  if (typeof trackTimer.enabled == "undefined") { //init
    trackTimer.time = [0,0];
    trackTimer.enabled = false;
  }

  updateProgress(trackTimer.time);
  if (trackTimer.enabled===true) {
    if (trackTimer.time[0]<trackTimer.time[1]) {
      trackTimer.time[0] = trackTimer.time[0]+1;
    } else {
      trackTimer.enabled = false;
    }
  }
  setTimeout(trackTimer, 1000);
}
// --- helper methods ---

function fadeOutIn(id) {
  $(id).fadeOut('fast', function() {
    $(id).fadeIn('slow', null);
  });
}

function setClass(id, name, val) {
  if (val === true) {
    $(id).addClass(name);
  } else {
    $(id).removeClass(name);
  }
}
function setBtnActive(id, val) { setClass(id, 'active', val); }
function setBtnEnabled(id, val) { setClass(id, 'disabled', !val); }

function togglePlayIcon(id, b) {
    if (b) {
      $(id).removeClass('glyphicon-pause').addClass('glyphicon-play');
    } else {
      $(id).removeClass('glyphicon-play').addClass('glyphicon-pause');
    }
}
function updatePauseButton() { togglePlayIcon('#btnpause-icon', MPD.status.stState!="Playing"); }

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
    if (index==null || (MPD.status && MPD.status.stState=="Stopped"))
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
  r += '<td class="sort-drag" style="cursor:move;">'+(song.sgIndex+1)+'</td>';
  r+='<td onclick="onQueuePlay('+song.sgIndex+')" style="cursor:pointer;">';
  if (artist != '')
    r += artist + ' - ';
  r += title + '</td><td>' + dur + '</td>';
  r += '<td>' +icon('trash','onQueueTrash('+song.sgIndex+');fadeOutIn(this);') +'</td>';
  r += '</tr>';
  return r;
}

// similar but for dir view
function rowFromDirSong(i, song) {
  var tags = song.sgTags;
  var artist = getTag(tags.Artist);
  var file = song.sgFilePath.replace(/^.*[\\\/]/, '');
  var title = getTag(tags.Title, file);
  var dur = secToTime(parseInt(song.sgLength, 10));
  var r = '<tr>'
  r += '<td>'+icon('music')+'</td>';
  r+='<td onclick="onDirAddClick('+i+');fadeOutIn(this);" style="cursor:pointer;">';
  if (artist != '')
    r += artist + ' - ';
  r += title + '</td><td>' + dur + '</td>';
  r += '<td></td>';
  r += '</tr>';
  return r;
}

// helper to generate a row for the outputs
function rowFromOutput(output) {
  var name = output.dOutputName;
  var val = output.dOutputEnabled;
  var iconname = val ? 'volume-up' : 'volume-off';
  var r = '<tr>'
  r += '<td>'+name+'</td>';
  r += '<td>' +icon(iconname,'onOutputSet('+output.dOutputID+','+(!val)+');') +'</td>';
  r += '</tr>';
  return r;
}

function rowFromDirEntry(i, item) {
  if (item.tag=='LsDirectory') {
    var name=item.contents.replace(/^.*\//,'');
    var r ='<tr><td>'+icon('folder-open')+'</td>';
    r += '<td><a onclick="onDirEnterClick('+i+');" style="cursor:pointer;">'
      +name+'</a></td><td /><td>'+icon('plus','onDirAddClick('+i+');fadeOutIn(this);')+'</td>';
    return r;
  } else if (item.tag=='LsPlaylist') { //we dont include playlists in dir tree
    return '';
  } else if (item.tag=='LsSong') {
    return rowFromDirSong(i, item.contents);
  }
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
    });
  });
}

function updateOutputs() {
  mpdget('outputs', function(data) {
    MPD.outputs = JSON.parse(data); 
    var outputs = MPD.outputs;
    $('#outputs > tbody').html('');
    $.each(outputs, function(i, output) {
      $('#outputs > tbody').append(rowFromOutput(output));
    });
  });
}

function updateBreadcrumb() {
  var path = 'path/'+MPD.currdir.join(',');
  mpdget(path, function(data) {
    var path = JSON.parse(data).split('/');
    $('#path').html('');
    $('#path').append('<li><a style="cursor:pointer;" onclick="onCrumbClick(0)">root</a></li>');
    $.each(path, function(i, item) {
      $('#path').append('<li><a style="cursor:pointer;" '
          +'onclick="onCrumbClick('+(i+1)+')">'+item+'</a></li>');
    });
  });
}

function updateDirectory() {
  var path = 'browse/'+MPD.currdir.join(',');
  mpdget(path, function(data) {
    var list = JSON.parse(data);
    updateBreadcrumb();
    $('#directory > tbody').html('');
    $.each(list, function(i, item) {
      $('#directory > tbody').append(rowFromDirEntry(i,item));
    });
  });
}

//long-polling loop to hampc server
//Server says approximately what changed, the callback decides what requests are necessary
function doPoll() { setTimeout(poll, 100); }
function poll() {
  $.get(withMPDpref('ping'), function(data) { //test connection to hampc server
      var ret = JSON.parse(data);
      if (typeof ret.error != "undefined")
        return; //still problem

      //if we had a connection problem, but now it is gone
      //-> update everything to prevent glitches
      if ($('#notify').is(':visible')) {
        $('#notify').text('').removeClass().addClass('alert').hide(); //clear error message
        updateInterface(); //get new data
      }
  });
  $.ajax({ //long poll
    url: withMPDpref('idle/'+MPD.lasttime),
    type: 'GET',
    async: true,
    cache: false,
    dataType: "json",
    timeout: 30000,
    complete: doPoll,
    error: function(jqXHR, text, error) {
      if (text!='timeout') { //connection problem!
        $('#notify').show().addClass('alert-danger').text('No connection to hampc!');
        trackTimer.enabled = false;
      }
    },
    success: function(data) {
      //get changed subsystems and timestamp
      var ret = JSON.parse(data);
      var subsystems = ret[0];
      MPD.lasttime = ret[1];

      if (subsystems.length == 0) { //hampc has no connection to MPD! Otherwise idle is never []
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
            updateControls(); //because of volume
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

// -- helpers to communicate with hampc server --

//generate a full request url
function withMPDpref(str) { return 'mpd/'+MPD.password+'/'+str; }

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

// load a panel, hide the others, highlight correct button
function loadTabCallback(buttonid, panelid) {
  return function() {
    $('.panel-primary').hide();
    $('.navbar-nav').children('li').removeClass('active');
    $(buttonid).addClass('active');
    $(panelid).show();
  };
}

function onPreviousClick() {
  MPDexec('previous');
}
function onNextClick() {
  MPDexec('next');
}
function onStopClick() {
  MPDexec('stop');
}
function onPauseClick() {
  trackTimer.enabled ? MPDexec('pause/True') : MPDexec('play'); //send request
  MPD.status.stState = MPD.status.stState=="Playing" ? "Paused" : "Playing";
  trackTimer.enabled = !trackTimer.enabled;
  updatePauseButton(); //fake it (for UI experience)

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


function onQueueTrash(index) {
  MPDexec('delete/'+index);
}
function onQueuePlay(index) {
  MPDexec('play/'+index);
}

var deb
function onQueueDrag(ev) {
  $('#queue').attr('data-previndex', $(ev.item).index());
}
function onQueueDrop(ev) {
  var oldi = $('#queue').attr('data-previndex');
  var newi = $(ev.item).index();
  MPDexec('move/'+oldi+'/'+newi); //move item
  //cleanup
  $('#queue').removeAttr('data-previndex');
}

function onDirEnterClick(i) {
  MPD.currdir.push(i);
  updateDirectory();
  window.scrollTo(0,0);
}

function onDirAddClick(i) { MPDexec('add/'+MPD.currdir.join(',')+','+i); }

function onCrumbClick(index) {
  MPD.currdir = MPD.currdir.slice(0,index);
  updateDirectory();
  window.scrollTo(0,0);
}

function onPwChange() {
  $('#panelPassword').removeClass('panel-success').addClass('panel-default');
}
function onSetPwClick() {
  MPD.password = $('#pw').val();
  $.localStorage.set('mpdpw', MPD.password);
  $('#panelPassword').removeClass('panel-default').addClass('panel-success');
}

function onOutputSet(id, val) { MPDexec('outputs/'+id+'/'+(val?'True':'False')); }

function onStreamClick() {
  var player = $('#stream-jplayer');
  if ($('#btnstream-icon').hasClass('glyphicon-play')) {
    player.jPlayer('play');
  } else {
    player.jPlayer('stop');
  }
  togglePlayIcon('#btnstream-icon', !$('#btnstream-icon').hasClass('glyphicon-play'));
}
