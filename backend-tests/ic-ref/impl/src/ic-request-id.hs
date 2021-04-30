{-# LANGUAGE OverloadedStrings #-}
module Main where

import Options.Applicative
import Control.Monad (join)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Text.Hex as H
import qualified Data.HashMap.Lazy as HM
import System.IO


import IC.HTTP.CBOR
import IC.HTTP.GenR
import IC.HTTP.RequestId

work :: Maybe FilePath -> IO ()
work input = do
  request <- maybe BS.getContents BS.readFile input
  case decode request of
    Left err -> do
        T.hPutStrLn stderr "Failed to decode CBOR:"
        T.hPutStrLn stderr err
    Right (GRec m) | Just content <- HM.lookup "content" m ->
        T.putStrLn $ H.encodeHex $ BS.toStrict $ requestId content
    Right gr -> do
        T.hPutStrLn stderr "Request does not look like an envelop (could not find field \"content\"):"
        T.hPutStrLn stderr (T.pack (show gr))

main :: IO ()
main = join . customExecParser (prefs showHelpOnError) $
  info (helper <*> parser)
  (  fullDesc
  <> header "Internet Computer request id"
  <> progDesc "Given a CBOR-encoded request with envelope, calculate the request id"
  )
  where
    parser :: Parser (IO ())
    parser =
      work
        <$> optional (strArgument
            (  metavar "FILE"
            <> help "file to read (default: stdin)"
            ))
