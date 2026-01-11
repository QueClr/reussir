{- |
Module      : Reussir.Core.Mangle
Description : Name mangling scheme for Reussir.

We borrow most of the ideas from rust's v0 symbol format.
-}
module Reussir.Core.Mangle where

import Data.Text.Builder.Linear as TB

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
