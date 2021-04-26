{-# LANGUAGE OverloadedStrings #-}
import Options.Applicative
import Data.Foldable
import Control.Concurrent
import Control.Monad (join, forever)
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Handler.Warp
import qualified Data.Text as T
import IC.HTTP
import IC.Version
import qualified IC.Crypto.BLS as BLS

defaultPort :: Port
defaultPort = 8001


work :: Maybe Int -> Maybe FilePath -> Maybe FilePath -> Bool ->  IO ()
work portToUse writePortTo backingFile log = do
    putStrLn "Starting ic-ref..."
    BLS.init
    withApp backingFile $ \app -> do
        let app' = if log then logStdoutDev app else app
        case portToUse of
          Nothing ->
            withApplicationSettings settings (pure app') $ \port -> do
              greet port
              forever (threadDelay maxBound)
          Just port -> do
            greet port
            runSettings (setPort port settings) app'
  where
    greet port = do
       putStrLn $ "Running at http://127.0.0.1:" ++ show port ++ "/"
       for_ writePortTo $ \fn -> writeFile fn (show port)

    settings = setHost "127.0.0.1" defaultSettings

main :: IO ()
main = join . customExecParser (prefs showHelpOnError) $
  info (helper <*> versions <*> parser)
  (  fullDesc
  <> header ("Internet Computer reference implementation " <> T.unpack implVersion)
  <> progDesc (
    "A stand-alone local reference implementation of the Internet Computer. \
    \By default, it listens on http://127.0.0.1:" ++ show defaultPort ++ "/. You \
    \can change the port with --pick-port or --listen-port.")
  )
  where
    versions :: Parser (a -> a)
    versions =
          infoOption (T.unpack implVersion) (long "version" <> help "show version number")
      <*> infoOption (T.unpack specVersion) (long "spec-version" <> help "show spec version number")
    parser :: Parser (IO ())
    parser = work
      <$>
        ( flag' Nothing
          (  long "pick-port"
          <> help ("pick a free port (instead of binding to 127.0.0.1:" ++ show defaultPort ++ ")")
          )
        <|>
          (Just <$>
            option auto
            (  long "listen-port"
            <> help "specify the listen port"
            )
          )
        <|> pure (Just defaultPort)
        )
      <*> optional (strOption
          (  long "write-port-to"
          <> help "write port to the given file"
        ))
      <*> optional (strOption
          (  long "state-file"
          <> metavar "FILE"
          <> help "file to persist IC state in"
        ))
      <*> switch
          (  long "http-log"
          <> help "print a HTTP log to stdout"
          )
