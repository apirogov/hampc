{-# LANGUAGE OverloadedStrings #-}
module Hampc.MPDHttpRestream where
import           Data.List (find)
import           Data.Maybe (isJust)
import           Control.Applicative
import           Control.Monad.Trans (liftIO)

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import qualified Blaze.ByteString.Builder.ByteString as BB
import qualified Data.Text.Lazy.Encoding as TL

import           Network.Wai                          (Request(requestHeaders))
import           Network.Http.Types                   (retrieveHeaders)
import           Network.HTTP.Types                   (StdMethod(..))
import           Network.URI                          (parseURI, URI(..), URIAuth(..))
import           Web.Scotty

import qualified Network.Http.Client as C
import qualified System.IO.Streams as S

splitUri uri = (domain, port, path)
  where auth   = uriAuthority uri
        domain = BS.pack $ maybe "" uriRegName auth
        port   = maybe 80 (read.tail.(\s -> if null s then ":80" else s).uriPort) auth
        path   = uriPath uri

--TODO: make it work with icy-metadata: http://www.smackfu.com/stuff/programming/shoutcast.html
returnStream Nothing _ _ _ = return ()
returnStream (Just uri) askIcy send flush = C.withConnection (C.openConnection domain port) $ (\c -> do
  r <- C.buildRequest $ --do
    C.http C.GET $ BS.pack path
    -- when askIcy $ C.setHeader "Icy-MetaData" "1"
  C.sendRequest c r C.emptyBody
  putStrLn "Start re-streaming."
  C.receiveResponseRaw c (streamChunker send flush)
  C.closeConnection c
  ) where (domain, port, path) = splitUri uri

returnStreamHeaders Nothing   = return []
returnStreamHeaders (Just uri) = C.withConnection (C.openConnection domain port) $ (\c -> do
  r <- C.buildRequest $ --do
    C.http C.HEAD $ BS.pack path
    -- when askIcy $ C.setHeader "Icy-MetaData" "1"
  C.sendRequest c r C.emptyBody
  headers <- retrieveHeaders . C.getHeaders <$> C.receiveResponse c (\r _ -> return r)
  C.closeConnection c
  return $ map (\(a,b)->(TL.decodeUtf8 $ BL.fromStrict a,TL.decodeUtf8 $ BL.fromStrict b)) headers
  ) where (domain, port, path) = splitUri uri

streamChunker send flush response istream = do
          chunk <- S.read istream
          case chunk of
            Nothing -> return ()
            Just c -> do
              send $ BB.fromByteString c
              streamChunker send flush response istream

-- re-stream from MPD over GET (act as kind-of pass-through)
streamRoute url = do
  headers <- liftIO $ returnStreamHeaders $ parseURI url
  let streamhead = mapM_ (\(a,b)-> addHeader a b) headers
      routestr = regex "^/stream/(.*)$"

  addroute HEAD routestr $ streamhead >> raw BL.empty
  get routestr $ do
    askIcy <- isJust . find (=="Icy-MetaData") . map fst . requestHeaders <$> request
    streamhead >> stream (returnStream (parseURI url) askIcy)

