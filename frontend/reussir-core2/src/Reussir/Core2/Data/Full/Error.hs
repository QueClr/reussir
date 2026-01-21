module Reussir.Core2.Data.Full.Error where

import Data.Int (Int64)
import Data.Vector.Strict qualified as V

import Reussir.Core2.Data.Semi.Type qualified as Semi
import Reussir.Core2.Data.UniqueID (GenericID)
import Reussir.Parser.Types.Capability qualified as Syn
import Reussir.Parser.Types.Lexer (Path (..))

data Error = Error
    { errorSpan :: (Int64, Int64)
    , errorKind :: ErrorKind
    }

data ErrorKind
    = InvalidRecordField
        { recordPath :: Path -- The path of the record
        , recordTypeArgs :: V.Vector Semi.Type -- The type arguments of the record
        , errorIndex :: Int -- The index of the field that is invalid
        }
    | InvalidNullableType Semi.Type
    | UnknownGeneric GenericID
    | InvalidCapability Path Syn.Capability
