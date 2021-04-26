{-# LANGUAGE TypeApplications #-}
module IC.Crypto.DER.Decode (safeDecode) where

import qualified Data.ByteString.Lazy as BS
import Data.ASN1.Types
import Data.ASN1.Encoding
import Data.ASN1.BinaryEncoding
import Data.Bifunctor

import Control.Exception
import System.IO.Unsafe
import Control.Monad
import Control.Seq

-- Works around https://github.com/vincenthz/hs-asn1/issues/41
safeDecode :: BS.ByteString -> Either String [ASN1]
safeDecode bs = unsafePerformIO $ do
    let r = first show $ decodeASN1 DER bs
    join . first show <$>
      try @SomeException (evaluate (r `using` seqFoldable (seqList rseq)))
