module IC.HTTP.RequestId (requestId) where

import Numeric.Natural
import IC.HTTP.GenR
import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.List (sort)
import Data.Serialize.LEB128
import IC.Hash

type RequestId = BS.ByteString

requestId :: GenR -> RequestId
requestId (GRec hm) = sha256 $ BS.concat $ sort $ map encodeKV $ HM.toList hm
requestId _ = error "requestID: expected a record"

encodeKV :: (T.Text, GenR) -> BS.ByteString
encodeKV (k,v) = sha256 (encodeText k) <> sha256 (encodeVal v)

encodeVal :: GenR -> BS.ByteString
encodeVal (GBlob b) = b
encodeVal (GText t) = encodeText t
encodeVal (GNat n) = encodeNat n
encodeVal (GRec _) = error "requestID: Nested record"
encodeVal (GList vs) = BS.concat $ map (sha256 . encodeVal) vs

encodeText :: T.Text -> BS.ByteString
encodeText = BS.fromStrict . T.encodeUtf8

encodeNat :: Natural -> BS.ByteString
encodeNat = BS.fromStrict . toLEB128
