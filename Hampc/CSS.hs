{-# LANGUAGE OverloadedStrings #-}
module Hampc.CSS (layoutCss) where
import Prelude hiding ((**))

import           Clay
import           Data.Text.Lazy (Text)

layoutCss :: Text
layoutCss = renderWith pretty [] $ do
  body ? minHeight (px 2000)
  element "a:focus" ? outlineStyle none
  element ".starter-template" ? marginTop (px 100)
  element ".navbar-form" |> ".form-group" |> "input" ? marginRight (px 5)
  element ".slider-selection" ? background (rgb 186 186 186)
  element "#volumebar" ? height (px 15)
  element "#progressbar" ? do
    width $ pct 80
    height $ px 15
    marginTop $ px 3
  element "#title" ? fontSize (px 35)
  element "#time" ? fontSize (px 24)
  element "#album,#artist" ? fontSize (px 18)
  element "#btnstream" ? marginTop (px 5)
  element "#streamurl" ? display displayNone
  element "#notify" ? display displayNone
  element "#panelBrowse" ? display displayNone
  element "#panelSettings" ? display displayNone
  element "table" ** "th:first-child,th:last-child,td:first-child,td:last-child" ? width (px 30)
  element "table" ** "th:nth-child(3),td:nth-child(3)" ? textAlign (alignSide sideRight)
