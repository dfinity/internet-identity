{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}

module IC.Wasm.Imports where

import Data.Int
import Data.Word
import IC.Wasm.Winter
import Text.Printf
import Control.Monad.Except


class WasmArg a where
    valueType :: ValueType
    fromValue :: Value -> Either String a
    toValue :: a -> Value

class WasmArgs a where
    stackType :: StackType
    fromValues :: [Value] -> Either String a
    toValues :: a -> [Value]

    default stackType :: WasmArg a => StackType
    stackType = [valueType @a]
    default fromValues :: WasmArg a => [Value] -> Either String a
    fromValues [x] = fromValue x
    fromValues xs = argError 1 xs
    default toValues :: WasmArg a => a -> [Value]
    toValues x = [toValue x]

argError :: Int -> [a] -> Either String b
argError n xs = Left $
    printf "expected %d arguments, got %d arguments" n (length xs)

instance WasmArg Int32 where
  valueType = I32Type
  fromValue (I32 i) = Right i
  fromValue v = Left $ "expected i32, got " ++ show v
  toValue = I32
instance WasmArgs Int32 where

instance WasmArg Word64 where
  valueType = I64Type
  fromValue (I64 i) = Right (fromIntegral i)
  fromValue v = Left $ "expected i64, got " ++ show v
  toValue = I64 . fromIntegral
instance WasmArgs Word64 where

instance WasmArgs () where
    stackType = []
    fromValues [] = Right ()
    fromValues xs = argError 0 xs
    toValues () = []

-- The formatting is a bit odd, but it allows to create new instances easily by copy and paste and adding lines
instance
    ( WasmArg a1
    , WasmArg a2
    ) => WasmArgs
    ( a1
    , a2
    ) where
    stackType =
        [ valueType @a1
        , valueType @a2
        ]
    fromValues
        [ x1
        , x2
        ] = (,)
            <$> fromValue x1
            <*> fromValue x2
    fromValues xs = argError 2 xs
    toValues
        ( x1
        , x2
        ) =
        [ toValue x1
        , toValue x2
        ]

instance
    ( WasmArg a1
    , WasmArg a2
    , WasmArg a3
    , WasmArg a4
    ) => WasmArgs
    ( a1
    , a2
    , a3
    , a4
    ) where
    stackType =
        [ valueType @a1
        , valueType @a2
        , valueType @a3
        , valueType @a4
        ]
    fromValues
        [ x1
        , x2
        , x3
        , x4
        ] = (,,,)
            <$> fromValue x1
            <*> fromValue x2
            <*> fromValue x3
            <*> fromValue x4
    fromValues xs = argError 3 xs
    toValues
        ( x1
        , x2
        , x3
        , x4
        ) =
        [ toValue x1
        , toValue x2
        , toValue x3
        , toValue x4
        ]

instance
    ( WasmArg a1
    , WasmArg a2
    , WasmArg a3
    ) => WasmArgs
    ( a1
    , a2
    , a3
    ) where
    stackType =
        [ valueType @a1
        , valueType @a2
        , valueType @a3
        ]
    fromValues
        [ x1
        , x2
        , x3
        ] = (,,)
            <$> fromValue x1
            <*> fromValue x2
            <*> fromValue x3
    fromValues xs = argError 3 xs
    toValues
        ( x1
        , x2
        , x3
        ) =
        [ toValue x1
        , toValue x2
        , toValue x3
        ]

instance
    ( WasmArg a1
    , WasmArg a2
    , WasmArg a3
    , WasmArg a4
    , WasmArg a5
    , WasmArg a6
    , WasmArg a7
    , WasmArg a8
    ) => WasmArgs
    ( a1
    , a2
    , a3
    , a4
    , a5
    , a6
    , a7
    , a8
    ) where
    stackType =
        [ valueType @a1
        , valueType @a2
        , valueType @a3
        , valueType @a4
        , valueType @a5
        , valueType @a6
        , valueType @a7
        , valueType @a8
        ]
    fromValues
        [ x1
        , x2
        , x3
        , x4
        , x5
        , x6
        , x7
        , x8
        ] = (,,,,,,,)
            <$> fromValue x1
            <*> fromValue x2
            <*> fromValue x3
            <*> fromValue x4
            <*> fromValue x5
            <*> fromValue x6
            <*> fromValue x7
            <*> fromValue x8
    fromValues xs = argError 88888888 xs
    toValues
        ( x1
        , x2
        , x3
        , x4
        , x5
        , x6
        , x7
        , x8
        ) =
        [ toValue x1
        , toValue x2
        , toValue x3
        , toValue x4
        , toValue x5
        , toValue x6
        , toValue x7
        , toValue x8
        ]

instance
    ( WasmArg a1
    , WasmArg a2
    , WasmArg a3
    , WasmArg a4
    , WasmArg a5
    , WasmArg a6
    , WasmArg a7
    , WasmArg a8
    , WasmArg a9
    , WasmArg a10
    ) => WasmArgs
    ( a1
    , a2
    , a3
    , a4
    , a5
    , a6
    , a7
    , a8
    , a9
    , a10
    ) where
    stackType =
        [ valueType @a1
        , valueType @a2
        , valueType @a3
        , valueType @a4
        , valueType @a5
        , valueType @a6
        , valueType @a7
        , valueType @a8
        , valueType @a9
        , valueType @a10
        ]
    fromValues
        [ x1
        , x2
        , x3
        , x4
        , x5
        , x6
        , x7
        , x8
        , x9
        , x10
        ] = (,,,,,,,,,)
            <$> fromValue x1
            <*> fromValue x2
            <*> fromValue x3
            <*> fromValue x4
            <*> fromValue x5
            <*> fromValue x6
            <*> fromValue x7
            <*> fromValue x8
            <*> fromValue x9
            <*> fromValue x10
    fromValues xs = argError 10 xs
    toValues
        ( x1
        , x2
        , x3
        , x4
        , x5
        , x6
        , x7
        , x8
        , x9
        , x10
        ) =
        [ toValue x1
        , toValue x2
        , toValue x3
        , toValue x4
        , toValue x5
        , toValue x6
        , toValue x7
        , toValue x8
        , toValue x9
        , toValue x10
        ]

instance
    ( WasmArg a1
    , WasmArg a2
    , WasmArg a3
    , WasmArg a4
    , WasmArg a5
    , WasmArg a6
    , WasmArg a7
    , WasmArg a8
    , WasmArg a9
    , WasmArg a10
    , WasmArg a11
    , WasmArg a12
    ) => WasmArgs
    ( a1
    , a2
    , a3
    , a4
    , a5
    , a6
    , a7
    , a8
    , a9
    , a10
    , a11
    , a12
    ) where
    stackType =
        [ valueType @a1
        , valueType @a2
        , valueType @a3
        , valueType @a4
        , valueType @a5
        , valueType @a6
        , valueType @a7
        , valueType @a8
        , valueType @a9
        , valueType @a10
        , valueType @a11
        , valueType @a12
        ]
    fromValues
        [ x1
        , x2
        , x3
        , x4
        , x5
        , x6
        , x7
        , x8
        , x9
        , x10
        , x11
        , x12
        ] = (,,,,,,,,,,,)
            <$> fromValue x1
            <*> fromValue x2
            <*> fromValue x3
            <*> fromValue x4
            <*> fromValue x5
            <*> fromValue x6
            <*> fromValue x7
            <*> fromValue x8
            <*> fromValue x9
            <*> fromValue x10
            <*> fromValue x11
            <*> fromValue x12
    fromValues xs = argError 12 xs
    toValues
        ( x1
        , x2
        , x3
        , x4
        , x5
        , x6
        , x7
        , x8
        , x9
        , x10
        , x11
        , x12
        ) =
        [ toValue x1
        , toValue x2
        , toValue x3
        , toValue x4
        , toValue x5
        , toValue x6
        , toValue x7
        , toValue x8
        , toValue x9
        , toValue x10
        , toValue x11
        , toValue x12
        ]


toImport ::
    forall a b s.
    (WasmArgs a, WasmArgs b) =>
    String -> String -> (a -> HostM s b) -> Import s
toImport mod_name fun_name  f = (mod_name, fun_name, stackType @a, stackType @b, f')
  where
    f' :: [Value] -> HostFunc s
    f' xs = do
      a <- withExceptT ((mod_name ++ "." ++ fun_name ++ ": ") ++) $
        ExceptT $ return (fromValues xs)
      b <- f a
      return $ toValues b

