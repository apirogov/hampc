{-# LANGUAGE OverloadedStrings #-}
import           Hampc.Site
import           Hampc.MPD
import           Hampc.MPDHttpRestream

import           Control.Monad (unless)
import           Options.Applicative
import           Data.Text.Lazy (pack)

import           Network.URI                          (isURI, uriPath, parseURI)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Network.Wai.Middleware.Static        (addBase, noDots, staticPolicy, (>->))
import           Network.Wai.Handler.Warp             (defaultSettings, setPort, run)
import           Network.Wai.Handler.WarpTLS          (certFile, defaultTlsSettings, keyFile, runTLS)
import           Network.Wai.Handler.WebSockets       (websocketsOr)
import           Network.WebSockets                   (defaultConnectionOptions)
import           Web.Scotty

data Args = Args {
              argHost :: String
            , argPort :: Integer
            , argBind :: Int
            , argURL  :: String
            , argUseTLS :: Bool
            , argTLSKey :: String
            , argTLSCrt :: String
            }

parseArgs :: Parser Args
parseArgs = Args
        <$> strOption   (long "mpdhost" <> short 'm' <> metavar "MPDHOST"
         <> help "Hostname of the MPD server" <> value "localhost" <> showDefault)
        <*> option auto (long "mpdport" <> short 'p' <> metavar "MPDPORT"
         <> help "Port of the MPD server" <> value 6600 <> showDefault)
        <*> option auto (long "port" <> short 'b' <> metavar "PORT"
         <> help "Port to bind hampc to" <> value 8080 <> showDefault)
        <*> strOption   (long "streamurl" <> short 'u' <> metavar "URL"
         <> help "Public HTTP stream URL" <> value "" <> showDefault)

        <*> switch      (long "secure" <> short 's' <> help "Use HTTPS")
        <*> strOption   (long "key" <> short 'k' <> metavar "KEYFILE"
         <> help "Server key for HTTPS" <> value "server.key" <> showDefault)
        <*> strOption   (long "crt" <> short 'c' <> metavar "CRTFILE"
         <> help "Server certificate for HTTPS" <> value "server.crt" <> showDefault)

--handle HTTP(S) requests
myScottyApp args = do
  let url = argURL args
      path = "/stream" ++ (maybe ("/"::String) uriPath $ parseURI url)
      strurl = if null url then "" else path
  middleware logStdoutDev -- for nice log output
  middleware $ staticPolicy (noDots >-> addBase "static") -- for pics, JS stuff

  get "/" $ html $ mainPage (pack strurl)             -- deliver web app
  mpdRoutes (argHost args) (argPort args)    -- MPD control API route
  streamRoute url

main :: IO ()
main = do
  args <- execParser $ info (helper <*> parseArgs) (fullDesc <> progDesc "Start the hampc client.")
  let prt = argBind args
      url = argURL args
      key = argTLSKey args
      crt = argTLSCrt args

  unless (null url || isURI url) $ error "invalid stream url!"

  httpApp <- scottyApp $ myScottyApp args
  let warpRun = if argUseTLS args --combine scotty+websockets, decide about TLS
                then runTLS (defaultTlsSettings { keyFile = key , certFile = crt })
                            (setPort prt defaultSettings)
                else run prt
  warpRun $ websocketsOr defaultConnectionOptions (websocketApp url) httpApp

