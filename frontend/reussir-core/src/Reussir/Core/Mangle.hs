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
-}
instance Manglable Path where
    mangle (Path prefix baseName) = undefined

{- | For ascii identifiers, the mangle is simply length+identifier.
| For unicode identifiers, the mangle is `u` + length + punycode'(identifier). where punycode' is the punycode encoding where `-` are replaced with `_`.
| If the identifier (or punycode) starts with a digit, add `_` between the length and the body.
-}
instance Manglable Identifier where
    mangle (Identifier name)
        | T.all isAscii name =
            -- ASCII: length + [_] + identifier
            let sep = if startsWithDigit name then TB.fromChar '_' else mempty
             in TB.fromDec (T.length name) <> sep <> TB.fromText name
        | otherwise =
            -- Unicode: u + length + [_] + punycode'(identifier)
            let encoded = TE.decodeASCII $ Punycode.encode name
                -- Replace '-' with '_' in the punycode output
                punycode' = T.map (\c -> if c == '-' then '_' else c) encoded
                sep = if startsWithDigit punycode' then TB.fromChar '_' else mempty
             in TB.fromChar 'u' <> TB.fromDec (T.length punycode') <> sep <> TB.fromText punycode'

-- | Check if a Text starts with a digit
startsWithDigit :: T.Text -> Bool
startsWithDigit t = case T.uncons t of
    Just (c, _) -> c >= '0' && c <= '9'
    Nothing -> False
