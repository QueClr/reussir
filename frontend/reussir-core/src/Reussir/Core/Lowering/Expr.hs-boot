{-# LANGUAGE OverloadedStrings #-}

module Reussir.Core.Lowering.Expr where

import Reussir.Core.Data.Full.Expr qualified as Full
import Reussir.Core.Data.Lowering.Context (ExprResult, LoweringEff)
import Reussir.Codegen.IR qualified as IR
import Reussir.Codegen.Value qualified as IR

lowerExpr :: Full.Expr -> LoweringEff ExprResult
lowerExprAsBlock ::
    Full.Expr ->
    [IR.TypedValue] ->
    (ExprResult -> LoweringEff ()) ->
    LoweringEff IR.Block
loadIfRef :: IR.TypedValue -> LoweringEff IR.TypedValue
