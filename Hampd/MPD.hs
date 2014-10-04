{-# LANGUAGE OverloadedStrings #-}
module Hampd.MPD where

import           Data.List (find)
import           Data.String (fromString)
import           Data.Maybe (fromMaybe)
import qualified Data.Text.Lazy as T
import           Control.Applicative
import           Control.Monad.Trans (liftIO)

import qualified Network.MPD as MPD
import           Web.Scotty

escapePath = map (\c-> if c=='\\' then '/' else c)

mpdRoutes :: String -> Integer -> ScottyM ()
mpdRoutes host port = do
    let mpdo a conv = do -- shortcut for boilerplate
          pw <- param "pw"
          val <- liftIO $ MPD.withMPDEx host port pw a
          case val of
            Left err -> return $ T.pack $ show err
            Right v -> return $ T.pack $ conv v

    get "/:pw/ping" $ mpdo MPD.ping show >>= html
    get "/:pw/current" $ mpdo MPD.currentSong show >>= html
    get "/:pw/status" $ mpdo MPD.status show >>= html
    get "/:pw/stats" $ mpdo MPD.stats show >>= html

    get "/:pw/ctrl/update" $ mpdo (MPD.update Nothing) show >>= html
    get "/:pw/ctrl/next" $ mpdo MPD.next show >>= html
    get "/:pw/ctrl/previous" $ mpdo MPD.previous show >>= html
    get "/:pw/ctrl/stop" $ mpdo MPD.stop show >>= html

    get "/:pw/ctrl/random" $ mpdo MPD.status (show . MPD.stRandom) >>= html
    get "/:pw/ctrl/random/:val" $ do
      b <- param "val"
      mpdo (MPD.random b) show >>= html

    get "/:pw/ctrl/play" $ mpdo (MPD.play Nothing) show >>= html
    get "/:pw/ctrl/play/:val" $ do
      p <- param "val"
      mpdo (MPD.play $ Just p) show >>= html

    get "/:pw/ctrl/pause" $ mpdo MPD.status (show . (==MPD.Paused) . MPD.stState) >>= html
    get "/:pw/ctrl/pause/:val" $ do
      b <- param "val"
      mpdo (MPD.pause b) show >>= html

    get "/:pw/ctrl/seek" $ mpdo MPD.status (show . MPD.stTime) >>= html
    get "/:pw/ctrl/seek/:val" $ do
      p <- param "val"
      mpdo (do id <- (fromMaybe (MPD.Id 0) . MPD.stSongID) <$> MPD.status
               MPD.seekId id p) show >>= html

    get "/:pw/ctrl/volume" $ mpdo MPD.status (show . (fromMaybe 0) . MPD.stVolume) >>= html
    get "/:pw/ctrl/volume/:val" $ do
      p <- param "val"
      mpdo (MPD.setVolume p) show >>= html

    get "/:pw/ctrl/crossfade" $ mpdo MPD.status (show . MPD.stXFadeWidth) >>= html
    get "/:pw/ctrl/crossfade/:val" $ do
      s <- param "val"
      mpdo (MPD.crossfade s) show >>= html

    get "/:pw/ctrl/repeat" $ mpdo MPD.status (show . MPD.stRepeat) >>= html
    get "/:pw/ctrl/repeat/:val" $ do
      b <- param "val"
      mpdo (MPD.repeat b) show >>= html

    get "/:pw/ctrl/single" $ mpdo MPD.status (show . MPD.stSingle) >>= html
    get "/:pw/ctrl/single/:val" $ do
      b <- param "val"
      mpdo (MPD.single b) show >>= html

    get "/:pw/ctrl/consume" $ mpdo MPD.status (show . MPD.stConsume) >>= html
    get "/:pw/ctrl/consume/:val" $ do
      b <- param "val"
      mpdo (MPD.consume b) show >>= html

    get "/:pw/ctrl/outputs" $ mpdo MPD.outputs show >>= html
    get "/:pw/ctrl/outputs/:index" $ do
      n <- param "index"
      mpdo ((maybe False MPD.dOutputEnabled
            . find (\MPD.Device{MPD.dOutputID=id} -> id==n))
            <$> MPD.outputs) show >>= html
    get "/:pw/ctrl/outputs/:index/:val" $ do
      n <- param "index"
      b <- param "val"
      mpdo ((if b then MPD.enableOutput else MPD.disableOutput) n) show >>= html

    get "/:pw/database/:path" $ do
      path <- param "path"
      mpdo (MPD.lsInfo $ fromString $ map (\c-> if c=='\\' then '/' else c) path) show >>= html

    get "/:pw/queue" $ mpdo (MPD.playlistInfo Nothing) show >>= html
    get "/:pw/queue/clear" $ mpdo MPD.clear show >>= html
    get "/:pw/queue/shuffle" $ mpdo (MPD.shuffle Nothing) show >>= html
    get "/:pw/queue/add/:path" $ do
      path <- param "path"
      mpdo (MPD.add $ fromString $ escapePath path) show >>= html
    get "/:pw/queue/delete/:index" $ do
      n <- param "index"
      mpdo (MPD.delete n) show >>= html
    get "/:pw/queue/move/:ind/:idest" $ do
      s <- param "ind"
      d <- param "idest"
      mpdo (MPD.move s d) show >>= html

-- TODO: return as JSON DATA, support for playlists, fix unicode paths in libmpd?
-- start client side
