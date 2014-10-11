{-# LANGUAGE OverloadedStrings #-}
import           Hampc.Site
import           Hampc.MPD

import           Options.Applicative

import           Web.Scotty
import           Web.Scotty.TLS
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Network.Wai.Middleware.Static        (addBase, noDots, staticPolicy, (>->))

data Args = Args {
              argHost :: String
            , argPort :: Integer
            , argBind :: Int
            , argTLS :: Bool
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

        <*> switch      (long "secure" <> short 's' <> help "Use HTTPS")
        <*> strOption   (long "key" <> short 'k' <> metavar "KEYFILE"
         <> help "Server key for HTTPS" <> value "server.key" <> showDefault)
        <*> strOption   (long "crt" <> short 'c' <> metavar "CRTFILE"
         <> help "Server certificate for HTTPS" <> value "server.crt" <> showDefault)

main :: IO ()
main = do
  args <- execParser $ info (helper <*> parseArgs) (fullDesc <> progDesc "Start the hampc client.")
  let runScotty = if argTLS args
            then scottyTLS (argBind args) (argTLSKey args) (argTLSCrt args)
            else scotty (argBind args)
  runScotty $ do
    middleware logStdoutDev
    middleware $ staticPolicy (noDots >-> addBase "static") -- for favicon.ico

    get "/" $ html mainPage
    mpdRoutes (argHost args) (argPort args)
