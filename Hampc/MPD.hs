{-# LANGUAGE OverloadedStrings, DeriveGeneric, StandaloneDeriving #-}
-- vim: set foldmethod=marker foldlevel=0:
module Hampc.MPD where
-- Imports
-------------------------------------------------------------------a------------ {{{
import           Data.Char (isDigit)
import           Data.List (find)
import           Data.String (fromString)
import           Data.Maybe (fromMaybe)
import           Data.Map (foldWithKey)
import           Control.Applicative
import           Control.Monad (foldM, forever)
import           Control.Monad.Trans (liftIO)
import           Control.Monad.Error (throwError)
import           Control.Concurrent

import           Control.Concurrent.STM
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           System.Timeout

import Data.ByteString.Internal (ByteString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

import qualified Network.MPD as MPD
import           Network.HTTP.Types (status204)
import           Data.Aeson hiding (json)
import           GHC.Generics (Generic)
import           Web.Scotty
------------------------------------------------------------------------------- }}}

-- Bunch of JSON instances, manual where deriving not possible
------------------------------------------------------------------------------- {{{
deriving instance Generic MPD.Device
instance ToJSON MPD.Device
deriving instance Generic MPD.Id
instance ToJSON MPD.Id
deriving instance Generic MPD.PlaylistName
instance ToJSON MPD.PlaylistName
deriving instance Generic MPD.LsResult
instance ToJSON MPD.LsResult
deriving instance Generic MPD.Metadata
instance ToJSON MPD.Metadata
deriving instance Generic MPD.State
instance ToJSON MPD.State
deriving instance Generic MPD.Stats
instance ToJSON MPD.Stats
deriving instance Generic MPD.Status
instance ToJSON MPD.Status
deriving instance Generic MPD.Subsystem
instance ToJSON MPD.Subsystem

instance ToJSON ByteString where
  toJSON = toJSON . T.decodeUtf8
instance ToJSON MPD.Path where
  toJSON = toJSON . MPD.toText
instance ToJSON MPD.Value where
  toJSON = toJSON . MPD.toText

deriving instance Generic MPD.Song
instance ToJSON MPD.Song where
  toJSON (MPD.Song {MPD.sgFilePath=fp, MPD.sgLength=len,
          MPD.sgId=sid, MPD.sgIndex=six, MPD.sgLastModified=lmod, MPD.sgTags=tags}) =
    object ["sgFilePath" .= MPD.toText fp, "sgLength" .= len, "sgLastModified" .= lmod,
             "sgId" .= sid, "sgIndex" .= six, "sgTags" .= (object $ foldWithKey f [] tags)]
    where f k v r = ((T.pack $ show (k::MPD.Metadata)) .= (toJSON v)):r
----------------------------------------------------------------------------------------------
-- }}}

-- enter the nth directory in given path, return new path
enterDir :: MPD.MonadMPD m => MPD.Path -> Int -> m MPD.Path
enterDir p n = do res <- MPD.lsInfo p
                  if n>=0 && n<length res
                  then case res !! n of
                         MPD.LsDirectory d -> return d
                         MPD.LsSong s -> return $ MPD.sgFilePath s
                         MPD.LsPlaylist _ -> throwError $ MPD.Custom "can not enter playlist"
                  else throwError $ MPD.Custom "invalid index"

-- get path corresponding to a comma-separated list of indices starting from root directory
getPath :: MPD.MonadMPD m => String -> m MPD.Path
getPath str = foldM enterDir (fromString "") splitted
  where splitted = if null str then []::[Int] else map (read . T.unpack)
                        $ filter (not . T.null) $ T.split (not.isDigit) (T.pack str)

listPlaylistById id = do
  l <- MPD.listPlaylists
  if id>=0 && id<length l
  then MPD.listPlaylistInfo (l !! id)
  else throwError $ MPD.Custom "invalid playlist id"

loadPlaylistById id = do
  l <- MPD.listPlaylists
  if id>=0 && id<length l
  then MPD.load (l !! id)
  else throwError $ MPD.Custom "invalid playlist id"

isOutputEnabled id = (maybe False MPD.dOutputEnabled
                     . find (\MPD.Device{MPD.dOutputID=n} -> n==id)) <$> MPD.outputs

setOutputState id b = (case b of
                      True -> MPD.enableOutput
                      False -> MPD.disableOutput) id

-- helper to execute MPD actions and return a response as Text
mpdexec :: ToJSON a => String -> Integer -> MPD.MPD a -> ActionM TL.Text
mpdexec host port a = do -- shortcut for boilerplate
          pw <- param "pw"
          val <- liftIO $ MPD.withMPDEx host port pw a
          case val of
              Left err -> return $ TL.decodeUtf8 $ encode $ object ["error" .= show err]
              Right v -> return $ TL.decodeUtf8 $ encode v

-- from monad-loops
iterateUntil :: Monad m => (a -> Bool) -> m a -> m a
iterateUntil p x = do
    y <- x
    if p y
        then return y
        else iterateUntil p x

mpdRoutes :: String -> Integer -> ScottyM ()
mpdRoutes host port = do
    let mpdo a = mpdexec host port a
        mpdo' a param = mpdo $ a param --allow passing an argument
        prefix = "/mpd/:pw"
        -- add an mpd route to execute an MPD action and return the result as JSON
        getmpd route action = get (fromString (prefix++route)) $ mpdo action >>= json
        -- same, but more flexible (pass an ActionM block), use mpdo/mpdo' inside
        getmpd' route action = get (fromString (prefix++route)) $ action >>= json

    --start infinite idle process on server side, share events with clients
    shared <- liftIO $ atomically $ newTVar ([], 0::Integer)
    liftIO $ forkIO $ forever $ do
      val <- MPD.withMPDEx host port "" $ MPD.idle []
      time <- (round <$> getPOSIXTime) :: IO Integer
      tex <- case val of
        Left _ -> return []
        Right v -> return v
      atomically $ writeTVar shared (tex, time)

    get (fromString (prefix++"/idle/:stamp")) $ do
      lastt <- param "stamp"
      val <- liftIO $ timeout (30*1000000) $ iterateUntil (\(_,t)-> t>(lastt::Integer)) $ do
        threadDelay 1000000
        atomically $ readTVar shared
      case val of
        Nothing -> status status204 >> html "" -- we stop idling for this client
        Just r  -> json $ TL.decodeUtf8 $ encode r

    getmpd "/ping" MPD.ping
    getmpd "/current" MPD.currentSong
    getmpd "/status" MPD.status
    getmpd "/stats" MPD.stats

    getmpd "/next" MPD.next
    getmpd "/previous" MPD.previous
    getmpd "/stop" MPD.stop
    getmpd "/update" $ MPD.update Nothing

    getmpd "/random" $ MPD.stRandom <$> MPD.status
    getmpd' "/random/:val" $ param "val" >>= mpdo' MPD.random

    getmpd "/play" $ MPD.play Nothing
    getmpd' "/play/:val" $ do
      p <- param "val"
      mpdo $ MPD.play $ Just p

    getmpd "/pause" $ (==MPD.Paused) . MPD.stState <$> MPD.status
    getmpd' "/pause/:val" $ param "val" >>= mpdo' MPD.pause

    getmpd "/seek" $ MPD.stTime <$> MPD.status
    getmpd' "/seek/:val" $ do
      p <- param "val"
      mpdo $ fromMaybe (MPD.Id 0) . MPD.stSongID <$> MPD.status >>= (flip MPD.seekId) p

    getmpd "/volume" $ (fromMaybe 0) . MPD.stVolume <$> MPD.status
    getmpd' "/volume/:val" $ param "val" >>= mpdo' MPD.setVolume

    getmpd "/crossfade" $ MPD.stXFadeWidth <$> MPD.status
    getmpd' "/crossfade/:val" $ param "val" >>= mpdo' MPD.crossfade

    getmpd "/repeat" $ MPD.stRepeat <$> MPD.status
    getmpd' "/repeat/:val" $ param "val" >>= mpdo' MPD.repeat

    getmpd "/single" $ MPD.stSingle <$> MPD.status
    getmpd' "/single/:val" $ param "val" >>= mpdo' MPD.single

    getmpd "/consume" $ MPD.stConsume <$> MPD.status
    getmpd' "/consume/:val" $ param "val" >>= mpdo' MPD.consume

    --path is comma-sep. list of indices
    getmpd' "/path/:path" $ do
      path <- param "path"
      mpdo $ getPath path
    getmpd' "/browse/:path" $ do
      path <- param "path"
      mpdo $ getPath path >>= MPD.lsInfo

    getmpd "/queue" $ MPD.playlistInfo Nothing
    getmpd "/shuffle" $ MPD.shuffle Nothing
    getmpd "/clear" MPD.clear

    getmpd' "/add/:path" $ do
      path <- param "path"
      mpdo $ getPath path >>= MPD.add

    getmpd' "/delete/:index" $ param "index" >>= mpdo' MPD.delete
    getmpd' "/move/:ind/:idest" $ do
      s <- param "ind"
      d <- param "idest"
      mpdo $ MPD.move s d

    getmpd "/outputs" MPD.outputs
    getmpd' "/outputs/:index" $ param "index" >>= mpdo' isOutputEnabled
    getmpd' "/outputs/:index/:val" $ do
      n <- param "index"
      b <- param "val"
      mpdo $ setOutputState n b

    getmpd "/playlists" MPD.listPlaylists
    getmpd' "/playlists/:id" $ param "id" >>= mpdo' listPlaylistById
    getmpd' "/playlists/:id/load" $ param "id" >>= mpdo' loadPlaylistById

-- TODO: support for queries? playlist creation/modification?
