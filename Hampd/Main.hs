{-# LANGUAGE OverloadedStrings #-}
import           Hampd.Site
import           Hampd.MPD

import           Options.Applicative

import           Web.Scotty
import           Web.Scotty.TLS

import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Network.Wai.Middleware.Static        (addBase, noDots, staticPolicy, (>->))

data Args = Args {
              argHost :: String
            , argPort :: Integer
            , argBind :: Int
            }

parseArgs :: Parser Args
parseArgs = Args
        <$> strOption   (long "mpdhost" <> short 'h' <> metavar "MPDHOST"
         <> help "Hostname of the MPD server" <> value "localhost" <> showDefault)
        <*> option auto (long "mpdport" <> short 'p' <> metavar "MPDPORT"
         <> help "Port of the MPD server" <> value 6600 <> showDefault)
        <*> option auto (long "port" <> short 'b' <> metavar "PORT"
         <> help "Port to bind hampd to" <> value 8080 <> showDefault)

main :: IO ()
main = do
  args <- execParser $ info (helper <*> parseArgs) (fullDesc <> progDesc "Start the hampd client.")

  scottyTLS (argBind args) "server.key" "server.crt" $ do
    middleware logStdoutDev
    middleware $ staticPolicy (noDots >-> addBase "static/images") -- for favicon.ico

    get "/" $ html mainPage
    mpdRoutes (argHost args) (argPort args)
