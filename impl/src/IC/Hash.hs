{-# LANGUAGE TypeApplications #-}
module IC.Hash where

import qualified Data.ByteString.Lazy as BS
import Crypto.Hash (hashlazy, SHA256, SHA224)
import Data.ByteArray (convert)

sha256 :: BS.ByteString -> BS.ByteString
sha256 = BS.fromStrict . convert . hashlazy @SHA256

sha224 :: BS.ByteString -> BS.ByteString
sha224 = BS.fromStrict . convert . hashlazy @SHA224
