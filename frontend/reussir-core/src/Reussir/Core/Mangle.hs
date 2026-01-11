{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Reussir.Core.Mangle
Description : Name mangling scheme for Reussir.

We borrow most of the ideas from rust's v0 symbol format.
-}
module Reussir.Core.Mangle where

import Data.Char (isAscii)
import Data.Text qualified as T
import Data.Text.Builder.Linear as TB
import Data.Text.Encoding qualified as TE
import Data.Text.Punycode qualified as Punycode
import Reussir.Core.Types.Type (FloatingPointType (..), IntegralType (..))
import Reussir.Parser.Types.Lexer

class Manglable a where
    mangle :: a -> TB.Builder

{- |
A base-62 number, used for encoding generic indices.

    0-9 maps to 0-9
    a-z maps to 10 to 35
    A-Z maps to 36 to 61

    base-62-number → { digit | lower | upper } _

Example:
    0	    _
    1	    0_
    11	    a_
    62	    Z_
    63	    10_
    1000	g7_
-}
newtype B62Num = B62Num Int

-- | Convert a digit value (0-61) to its base-62 character
digitToChar :: Int -> Char
digitToChar d
    | d < 10 = toEnum (fromEnum '0' + d)
    | d < 36 = toEnum (fromEnum 'a' + d - 10)
    | otherwise = toEnum (fromEnum 'A' + d - 36)

-- | Encode a non-negative integer to base-62 representation (without trailing _)
encodeB62 :: Int -> TB.Builder
encodeB62 n
    | n < 62 = TB.fromChar (digitToChar n)
    | otherwise = encodeB62 (n `div` 62) <> TB.fromChar (digitToChar (n `mod` 62))

instance Manglable B62Num where
    mangle (B62Num n)
        | n == 0 = TB.fromChar '_'
        | otherwise = encodeB62 (n - 1) <> TB.fromChar '_'

{- | For path mangling, we directly use the nested path mangling
scheme from v0 symbol format.
1. single root path: C + baseName
2. nested path: `Nv` + prefix + baseName

Example:
    1. C7example -> example
    2. NvC7mycrate7example -> mycrate::example
    3. NvNvC1a7example7exampla -> a::example::exampla
-}
instance Manglable Path where
    mangle (Path baseName segments) = go (reverse segments) baseName
      where
        go [] name =
            -- Root path: C + baseName
            TB.fromChar 'C' <> mangle name
        go (s : ss) name =
            -- Nested path: Nv + prefix + baseName
            TB.fromChar 'N' <> TB.fromChar 'v' <> go ss s <> mangle name

{- | For ascii identifiers, the mangle is simply length+identifier.
| For unicode identifiers, the mangle is `u` + length + punycode'(identifier). where punycode' is the punycode encoding where `-` are replaced with `_`.
| If the identifier (or punycode) starts with a digit or an underscore, add `_` between the length and the body.
-}
instance Manglable Identifier where
    mangle (Identifier name)
        | T.all isAscii name =
            -- ASCII: length + [_] + identifier
            let sep = if startsWithDigitOrUnderscore name then TB.fromChar '_' else mempty
             in TB.fromDec (T.length name) <> sep <> TB.fromText name
        | otherwise =
            -- Unicode: u + length + [_] + punycode'(identifier)
            let encoded = TE.decodeASCII $ Punycode.encode name
                -- Replace '-' with '_' in the punycode output
                punycode' = T.map (\c -> if c == '-' then '_' else c) encoded
                sep = if startsWithDigitOrUnderscore punycode' then TB.fromChar '_' else mempty
             in TB.fromChar 'u'
                    <> TB.fromDec (T.length punycode')
                    <> sep
                    <> TB.fromText punycode'

-- | Check if a Text starts with a digit
startsWithDigitOrUnderscore :: T.Text -> Bool
startsWithDigitOrUnderscore t = case T.uncons t of
    Just (c, _) -> (c >= '0' && c <= '9') || c == '_'
    Nothing -> False

instance Manglable FloatingPointType where
    mangle Float8 = "C2f8"
    mangle BFloat16 = "C4bf16"
    mangle (IEEEFloat 16) = "C3f16"
    mangle (IEEEFloat 32) = "f"
    mangle (IEEEFloat 64) = "d"
    mangle (IEEEFloat 128) = "C4f128"
    mangle (IEEEFloat _) = error "Unsupported floating point type"

instance Manglable IntegralType where
    mangle (Signed 8) = "a"
    mangle (Signed 16) = "s"
    mangle (Signed 32) = "l"
    mangle (Signed 64) = "x"
    mangle (Signed _) = error "Unsupported signed integer type"
    mangle (Unsigned 8) = "h"
    mangle (Unsigned 16) = "t"
    mangle (Unsigned 32) = "m"
    mangle (Unsigned 64) = "y"
    mangle (Unsigned _) = error "Unsupported unsigned integer type"
