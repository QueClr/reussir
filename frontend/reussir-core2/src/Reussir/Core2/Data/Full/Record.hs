module Reussir.Core2.Data.Full.Record where

import Data.HashTable.IO qualified as H
import Data.Text qualified as T
import Data.Vector.Strict qualified as V
import Reussir.Codegen.Context.Symbol (Symbol)
import Reussir.Codegen.Type.Data (Capability)
import Reussir.Core2.Data.Full.Type (Type)
import Reussir.Core2.Data.Semi.Record qualified as Semi
import Reussir.Core2.Data.Semi.Type qualified as Semi
import Reussir.Parser.Types.Lexer (Identifier, Path)

type FieldFlag = Bool

data RecordFields
    = Named (V.Vector (Identifier, Type, FieldFlag))
    | Unnamed (V.Vector (Type, FieldFlag))
    | Variants (V.Vector Symbol)
    deriving (Show, Eq)

data RecordKind
    = StructKind
    | EnumKind
    | EnumVariant {variantParent :: Symbol, variantIdx :: Int}
    deriving (Show, Eq)

data Record = Record
    { recordName :: Symbol -- Actualy name after mangle
    , recordRawPath :: Path -- The path before mangle
    , recordSemiTyParams :: [Semi.Type] -- The type parameter used in instantiation
    , recordFields :: RecordFields -- Instantiated fields
    , recordKind :: RecordKind -- Instantiated kind
    , recordDefaultCap :: Capability -- The default capability
    }
    deriving (Show, Eq)

type FullRecordTable = H.CuckooHashTable Symbol Record
type SemiRecordTable = H.CuckooHashTable Path Semi.Record

data InvalidRecInstantiation = InvalidRecInstantiation
    { invalidRecInstantiationPath :: Path
    , invalidRecInstantiationTyArgs :: [Semi.Type]
    , invalidRecInstantiationReason :: T.Text
    }
    deriving (Show, Eq)
