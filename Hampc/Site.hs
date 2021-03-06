module Hampc.Site where
import           Prelude                     hiding (div, head, id, span, null)
import           Data.Monoid                 ((<>))

import           Data.Text.Lazy              (toStrict, pack)
import           Hampc.CSS                   (layoutCss)

import           Text.Blaze.Html5
-- import           Text.Blaze.Html.Renderer.Text
import           Text.Blaze.Html.Renderer.Pretty
import           Text.Blaze.Html5.Attributes hiding (style, title, span, form)
import           Text.Blaze.Internal         (preEscapedText)

glyphicon icon = span ! class_ ("glyphicon glyphicon-"<>icon) $ ""
button' btnid icon a = button ! id btnid ! type_ "button" ! class_ "btn btn-default" $
                            glyphicon icon ! id (btnid <> "-icon") <> a

css url = link ! href url ! rel "stylesheet" ! media "screen"
script' url = script ! src url $ ""

layout :: Html -> Html -> Html
layout t b = docTypeHtml $ do
           head $ do
             meta ! charset "utf-8"
             meta ! httpEquiv "X-UA-Compatible" ! content "IE=edge,chrome=1"
             meta ! name "viewport" ! content "width=device-width, initial-scale=1"
             meta ! name "description" ! content "hampc - MPD web client"
             meta ! name "author" ! content "Anton Pirogov"

             title t

             css "css/bootstrap.min.css"
             css "css/bootstrap-theme.min.css"
             css "css/bootstrap-slider.css"

             link ! href "img/favicon.ico" ! rel "shortcut icon" ! type_ "image/vnd.microsoft.icon"
             style $ preEscapedText $ toStrict layoutCss

           body $ do
             navBar >> b

             script' "js/jquery.min.js"
             script' "js/jquery.storageapi.min.js"
             script' "js/jquery.jplayer.js"
             script' "js/bootstrap.min.js"
             script' "js/bootstrap-slider.js"
             script' "js/Sortable.min.js"
             script' "hampc.js"

navBar :: Html
navBar = div ! class_ "navbar navbar-inverse navbar-fixed-top" ! customAttribute "role" "navigation" $
          div ! class_ "container" $ do
            div ! class_ "navbar-header" $ do
              button ! type_ "button" ! class_ "navbar-toggle"
                     ! dataAttribute "toggle" "collapse" ! dataAttribute "target" ".navbar-collapse" $ do
                     span ! class_ "icon-bar" $ ""
                     span ! class_ "icon-bar" $ ""
                     span ! class_ "icon-bar" $ ""
              span ! class_ "navbar-brand" $
                img ! src "img/hampc.png" ! width "32" ! height "32" ! alt "hampc" >> (span $ "hampc")

            div ! class_ "navbar-collapse collapse" $ do
              ul ! class_ "nav navbar-nav" $ do
                li ! id "navqueue" ! class_ "active" $ a ! href "#" $ "Queue"
                li ! id "navbrowse" $ a ! href "#browse" $ "Browse"
                -- li ! id "navplaylists" $ a ! href "#playlists" $ "Playlists"
                li ! id "navsettings" $ a ! href "#settings" $ "Settings"

              div ! class_ "btn-toolbar navbar-btn navbar-right" ! customAttribute "role" "toolbar" $ do
                div ! class_ "btn-group" $ do
                  button' "btnprevious" "backward" ""
                  button' "btnstop" "stop" ""
                  button' "btnpause" "pause" ""
                  button' "btnnext" "forward" ""
                div ! class_ "btn-group" $
                  div ! class_ "btn btn-toolbar btn-default" $ do
                    glyphicon "volume-up"
                    div ! id "volume" ! dataAttribute "slider-min" "0" ! dataAttribute "slider-max" "100"
                        ! dataAttribute "slider-step" "5" ! dataAttribute "slider-id" "volumebar"$ ""

              form ! id "search" ! class_ "navbar-form navbar-right" ! customAttribute "role" "search" $
                div ! class_ "form-group" $
                  input ! type_ "text" ! class_ "form-control" ! placeholder "Search"

panelQueue =
            div ! class_ "panel panel-primary" ! id "panelQueue" $ do
              div ! class_ "panel-heading" $ b $ "Queue"
              div ! class_ "panel-body" $ do
                h1 $ do
                  glyphicon "play" ! id "state"
                  preEscapedText $ "&nbsp;&nbsp;"
                  span ! id "title" $ ""
                h4 $ do
                  span ! id "album" ! class_ "text" $ ""
                  span ! id "artist" ! class_ "text pull-right" $ ""
                span ! id "time" ! class_ "text pull-right" $ ""
                div ! id "progress" ! dataAttribute "slider-id" "progressbar" $ ""

              table ! id "queue" ! class_ "table table-hover" $ do
                thead $ tr $ th "#" >> th "Title" >> th "Duration" >> th ""
                tbody ""

panelBrowse = div ! class_ "panel panel-primary" ! id "panelBrowse" $ do
                div ! class_ "panel-heading" $ b $ "Database"
                ol ! id "path" ! class_ "breadcrumb" $ ""
                table ! id "directory" ! class_ "table table-hover" $ do
                  thead $ tr $ th "" >> th "Title" >> th "Duration" >> th ""
                  tbody ""

panelSettings = div ! class_ "panel panel-primary" ! id "panelSettings" $ do
                  div ! class_ "panel-heading" $ b $ "Settings"
                  div ! class_ "panel-body" $ do
                    div ! class_ "panel panel-success" ! id "panelPassword" $ do
                      div ! class_ "panel-heading" $ "MPD Password"
                      div ! class_ "panel-body" $
                        div ! class_ "input-group" $ do
                          input ! type_ "password" ! class_ "form-control" ! id "pw"
                          span ! class_ "input-group-btn" $
                            button ! class_ "btn btn-default" ! type_ "button" ! id "btnsetpw" $ "Set"
                    div ! class_ "panel panel-default" $ do
                      div ! class_ "panel-heading" $ "Outputs"
                      table ! id "outputs" ! class_ "table table-hover" $ do
                        thead $ tr $ th "Name" >> th ""
                        tbody ""

mainPage streamURL =
          pack $ renderHtml $ layout "hampc - home" $
            div ! class_ "container starter-template" $
              div ! class_ "row" $ do
                div ! class_ "col-md-10 col-xs-12" $ do
                  div ! class_ "alert" ! id "notify" $ ""
                  panelQueue >> panelBrowse >> panelSettings

                div ! class_ "col-md-2 col-xs-12" $
                  div ! class_ "btn-toolbar" $ do
                    div ! class_ "btn-group-vertical btn-block btn-group-lg" ! dataAttribute "toggle" "buttons" $ do
                      button' "btnrandom" "random" "Random"
                      button' "btnconsume" "fire" "Consume"
                      button' "btnsingle" "star" "Single"
                      button' "btnrepeat" "repeat" "Repeat"
                    div ! id "btn-responsive-block" ! class_ "btn-group-vertical btn-block btn-group-lg" $ do
                      button' "btnupdate" "refresh" "Update DB"
                      button' "btnclear" "trash" "Clear Queue"
                    if streamURL==""
                    then preEscapedText ""
                    else do
                      div ! id "streamurl" $ (preEscapedText $ toStrict $ pack streamURL)
                      div ! id "stream-jplayer" ! class_ "jp-jplayer" $ ""
                      div ! id "btn-responsive-block" ! class_ "btn-group-vertical btn-block btn-group-lg" $
                        button' "btnstream" "play" "Stream"

