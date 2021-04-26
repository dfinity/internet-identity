module IC.Id.Fresh where

import IC.Types
import IC.Id.Forms

import Data.ByteString.Builder
import Data.Word

-- Not particulary efficent, but this is a reference implementation, right?
freshId :: [EntityId] -> EntityId
freshId ids =
    head $
    filter (`notElem` ids) $
    map (EntityId . mkOpaqueId . toLazyByteString . word64LE)
    [1024::Word64 ..]
