{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}

-------------------------------------------------------------------------------
-- |
-- Module      : Language.Lambda.Utils
-- Description : Utility functions.
-- Copyright   : (c) Giuseppe Lomurno, 2018
-- License     : MIT
-- Maintainer  : Giuseppe Lomurno <lomurno.giuseppe97@gmail.com>
-- Stability   : experimental
-- Portability : non-portable
--
-------------------------------------------------------------------------------

module Language.Lambda.Utils where

import Data.Text.Prettyprint.Doc

import Language.Lambda.Types

initPrec, lambdaPrec, appPrec, appLeftPrec, appRightPrec, ifPrec :: Rational
initPrec = 0
lambdaPrec = 1
appPrec = 9
appLeftPrec = 8.9
appRightPrec = 9
ifPrec = 1

binOpPrecs :: BinOp arg res -> (Rational, Rational, Rational)
binOpPrecs PrimAdd    = (5, 4.9, 5)
binOpPrecs PrimSub    = (5, 4.9, 5)
binOpPrecs PrimMul    = (6, 5.9, 6)
binOpPrecs PrimDiv    = (6, 5.9, 6)
binOpPrecs PrimIntEq  = (4, 4, 4)
binOpPrecs PrimBoolEq = (4, 4, 4)
binOpPrecs PrimAnd    = (3, 3, 3)
binOpPrecs PrimOr     = (2, 2, 2)

unOpPrec :: Op arg res -> (Rational, Rational)
unOpPrec PrimNeg = (6, 6)
unOpPrec PrimNot = (6, 6)
unOpPrec PrimFst = (9, 9)
unOpPrec PrimSnd = (9, 9)

opPrec, opPrecArg :: Op arg res -> Rational
opPrec    (unOpPrec -> (x, _)) = x
opPrecArg (unOpPrec -> (_, x)) = x

binOpPrec, binOpLeftPrec, binOpRightPrec :: BinOp arg res -> Rational
binOpPrec      (binOpPrecs -> (x, _, _)) = x
binOpLeftPrec  (binOpPrecs -> (_, x, _)) = x
binOpRightPrec (binOpPrecs -> (_, _, x)) = x

maybeParens :: Bool -> Doc ann -> Doc ann
maybeParens True = parens
maybeParens False = id
