module Reussir.Core2.Full.Type where

import Data.HashTable.IO qualified as H
import Data.IntMap.Strict qualified as IntMap
import Data.Maybe (fromMaybe)
import Effectful (Eff, IOE, liftIO, (:>))
import Reussir.Codegen.Context.Symbol (verifiedSymbol)
import Reussir.Codegen.Type (Capability (..))
import Reussir.Core2.Data.Full.Record (SemiRecordTable)
import Reussir.Core2.Data.Full.Type (GenericMap, Type (..))
import Reussir.Core2.Data.Semi.Record qualified as Semi
import Reussir.Core2.Data.Semi.Type qualified as Semi
import Reussir.Core2.Data.UniqueID (GenericID (..))
import Reussir.Core2.Semi.Mangle (mangleABIName)
import Reussir.Core2.Semi.Type qualified as Semi
import Reussir.Parser.Types.Capability qualified as Syn

convertCapability :: Syn.Capability -> Capability
convertCapability Syn.Value = Value
convertCapability Syn.Shared = Shared
convertCapability Syn.Regional = Regional
convertCapability Syn.Flex = Flex
convertCapability Syn.Rigid = Rigid
convertCapability Syn.Field = Field
convertCapability Syn.Unspecified = Unspecified

convertSemiType :: (IOE :> es) => GenericMap -> SemiRecordTable -> Semi.Type -> Eff es Type
convertSemiType genericMap semiRecords semiType = case semiType of
    Semi.TypeHole _ -> error "Type hole is not allowed in conversion"
    Semi.TypeRecord path typeArgs flex -> do
        let instantiatedTyArgs = map (flip Semi.substituteGenericMap genericMap) typeArgs
            mangledSymbol = mangleABIName (Semi.TypeRecord path instantiatedTyArgs flex)
            symbol = verifiedSymbol mangledSymbol
        record <- liftIO $ H.lookup semiRecords path
        let defaultCap = fromMaybe Syn.Value (fmap Semi.recordDefaultCap record)
        case (defaultCap, flex) of
            (Syn.Value, _) -> pure $ TypeRecord symbol
            (Syn.Shared, Semi.Irrelevant) -> pure $ TypeRc (TypeRecord symbol) Shared
            (Syn.Regional, Semi.Regional) -> pure $ TypeRc (TypeRecord symbol) Regional
            (Syn.Regional, Semi.Flex) -> pure $ TypeRc (TypeRecord symbol) Flex
            (Syn.Regional, Semi.Rigid) -> pure $ TypeRc (TypeRecord symbol) Rigid
            _ -> error "Invalid capability for record"
    Semi.TypeIntegral int -> pure $ TypeIntegral int
    Semi.TypeFP fp -> pure $ TypeFP fp
    Semi.TypeBool -> pure TypeBool
    Semi.TypeStr -> pure TypeStr
    Semi.TypeUnit -> pure TypeUnit
    Semi.TypeClosure args ret -> do
        args' <- mapM (convertSemiType genericMap semiRecords) args
        ret' <- convertSemiType genericMap semiRecords ret
        pure $ TypeRc (TypeClosure args' ret') Shared -- Closure is always wrapped in Rc for now
    Semi.TypeGeneric (GenericID gid) -> case (IntMap.lookup (fromIntegral gid) genericMap) of
        Nothing -> error "Generic not found in conversion"
        Just ty -> convertSemiType genericMap semiRecords ty
    Semi.TypeBottom -> pure TypeBottom
    Semi.TypeNullable x -> TypeNullable <$> (convertSemiType genericMap semiRecords x)
