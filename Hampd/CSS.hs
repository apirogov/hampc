{-# LANGUAGE OverloadedStrings #-}
module Hampd.CSS (layoutCss) where
import Prelude hiding ((**))

import           Clay
import           Data.Text.Lazy (Text)

layoutCss :: Text
layoutCss = renderWith pretty [] $ do
  body ? minHeight (px 2000)
  element ".starter-template" ? marginTop (px 100)
  element "#search" ? marginRight 0
  element ".slider-selection" ? background (rgb 186 186 186)
  element "#volumebar" ? height (px 15)
  element "#progressbar" ? do
    width $ pct 80
    height $ px 15
    marginTop $ px 3
  element "#time" ? fontSize (pt 16)
  element "#notify" ? display displayNone
  element "#panelBrowse" ? display displayNone
  element "#panelSettings" ? display displayNone
  -- element "#queue" ** "th:nth-child(3)" ** "td:nth-child(3)" ? textAlign (alignSide sideRight)
