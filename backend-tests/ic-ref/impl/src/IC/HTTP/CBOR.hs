{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{- |
Encoding from generic requests/responses to/from CBOR
-}
module IC.HTTP.CBOR where

import IC.HTTP.GenR
import qualified Data.HashMap.Lazy as HM
import Codec.CBOR.Term
import Codec.CBOR.Write
import Codec.CBOR.Read
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy (ByteString)
import Data.Bifunctor
import qualified Data.Text as T
import Control.Monad

encode :: GenR -> Builder
encode r = toBuilder $ encodeTerm $ TTagged 55799 $ go r
  where
    go (GNat n) = TInteger (fromIntegral n)
    go (GText t) = TString t
    go (GBlob b) = TBytes (BS.toStrict b)
    go (GRec m) = TMap [ (TString k, go v) | (k,v) <- HM.toList m ]
    go (GList xs) = TList (map go xs)

decode :: ByteString -> Either T.Text GenR
decode s =
    first (\(DeserialiseFailure _ s) -> "CBOR decoding failure: " <> T.pack s)
        (deserialiseFromBytes decodeTerm s)
    >>= begin
  where
    begin (leftOver, _)
      | not (BS.null leftOver) = Left $ "Left-over bytes: " <> shorten 20 (T.pack (show leftOver))
    begin (_, TTagged 55799 t) = go t
    begin _ = Left "Expected CBOR request to begin with tag 55799"

    shorten :: Int -> T.Text -> T.Text
    shorten n s = a <> (if T.null b then "" else "…")
        where (a,b) = T.splitAt n s

    go (TInt n) | n < 0 = Left "Negative integer"
    go (TInt n) = return $ GNat (fromIntegral n)
    go (TInteger n) | n < 0 = Left "Negative integer"
    go (TInteger n) = return $ GNat (fromIntegral n)
    go (TBytes b) = return $ GBlob $ BS.fromStrict b
    go (TString t) = return $ GText t
    go (TMap kv) = goMap kv
    go (TMapI kv) = goMap kv
    go (TList vs) = GList <$> mapM go vs
    go (TListI vs) = GList <$> mapM go vs
    go t = Left $ "Unexpected term: " <> T.pack (show t)

    goMap kv = do
        tv <- mapM keyVal kv
        let hm = HM.fromList tv
        when (HM.size hm < length tv) $ Left "Duplicate keys in CBOR map"
        return (GRec hm)

    keyVal (TString k,v) = (k,) <$> go v
    keyVal _ = Left "Non-string key in CBOR map"
