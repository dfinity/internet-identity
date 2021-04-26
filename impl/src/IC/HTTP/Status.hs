{-# LANGUAGE OverloadedStrings #-}
-- | the response to the status request
module IC.HTTP.Status where

import IC.HTTP.GenR
import IC.Version
import IC.Ref
import IC.Crypto
import Data.HashMap.Lazy

r :: IC -> GenR
r ic = GRec $ mconcat
        [ "ic_api_version" =: GText specVersion
        , "impl_version" =: GText implVersion
        , "impl_source" =: GText "https://github.com/dfinity-lab/ic-ref"
        , "root_key" =: GBlob (toPublicKey (secretRootKey ic))
        ]
  where
    -- Convenient syntax
    (=:) = Data.HashMap.Lazy.singleton
