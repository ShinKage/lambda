{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

-------------------------------------------------------------------------------
-- |
-- Module      : Language.Lambda.AST
-- Description : Abstract syntax tree and relatives helper function
-- Copyright   : (c) Giuseppe Lomurno, 2018
-- License     : MIT
-- Maintainer  : Giuseppe Lomurno <lomurno.giuseppe97@gmail.com>
-- Stability   : experimental
-- Portability : non-portable
--
-------------------------------------------------------------------------------

module Language.Lambda.AST
  ( AST(..)
  , expType
  , lambda
  , letE
  , letrecE
  ) where

import Data.Kind
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Symbols.Unicode

import Language.Lambda.Types
import Language.Lambda.Utils
import Language.Lambda.Data.Singletons
import Language.Lambda.Data.Vec

-------------------------------------------------------------------------------
-- * Abstract Syntax Tree
-------------------------------------------------------------------------------

-- | The language Abstract Syntax Tree.
-- Using the provided constructors leads only to well-formed expressions.
-- Indexed by a list of binded types and the output type.
data AST :: forall n. Vec LType n -> LType -> Type where
  -- | An integer literal.
  IntE      :: Int -> AST env LInt
  -- | A boolean literal.
  BoolE     :: Bool -> AST env LBool
  -- | Unit literal.
  UnitE     :: AST env LUnit
  -- | Lambda expressions with explicit type, can be inferred thanks to 'SingI'
  -- instance.
  Lambda    :: SLType a -> AST (a :> env) b -> AST env (LArrow a b)
  -- | Variable with De Brujin indexes.
  Var       :: Elem env a -> AST env a
  -- | Lambda application.
  App       :: AST env (LArrow a b) -> AST env a -> AST env b
  -- | Fix operator, defines recursive functions.
  Fix       :: AST env (LArrow a a) -> AST env a
  -- | Conditional expressions, all branches must have the same return type.
  Cond      :: AST env LBool -> AST env a -> AST env a -> AST env a
  -- | Primitives binary operations.
  PrimBinOp :: AST env a -> BinOp a b -> AST env a -> AST env b
  -- | Primitives unary operations.
  PrimOp    :: Op arg res -> AST env arg -> AST env res
  -- | Build a pair of expressions.
  Pair      :: AST env a -> AST env b -> AST env (LProduct a b)
  -- | Build the left branch of a sum type.
  LeftE     :: AST env a -> AST env (LSum a b)
  -- | Build the right branch of a sum type.
  RightE    :: AST env b -> AST env (LSum a b)
  -- | Pattern matching on sum type.
  Case      :: AST env (LSum a b) -> AST env (LArrow a c) -> AST env (LArrow b c) -> AST env c

deriving instance Show (AST env ty)

instance Pretty (AST VNil ty) where
  pretty = prettyAST_

prettyAST_ :: AST env ty -> Doc ann
prettyAST_ e = snd $ go 0 initPrec e
  where go :: Int -> Rational -> AST env ty -> (Int, Doc ann)
        go i _ (IntE n)  = (i, pretty n)
        go i _ (BoolE b) = (i, pretty b)
        go i _ UnitE = (i, pretty "unit")
        go i prec (Lambda ty body) = case go i initPrec body of
          (i_body, doc_body) -> (i + 1, maybeParens (prec >= lambdaPrec) $
            fillSep [ pretty 'λ' <> pretty '#' <> pretty i_body <> pretty ':'
                                 <> pretty ty <> pretty '.'
                    , doc_body
                    ])
        go i _ (Var v) = (i, pretty '#' <> pretty (elemToInt v))
        go i prec (App body arg) = (i, maybeParens (prec >= appPrec) $
          snd (go i appLeftPrec body) <+> snd (go i appRightPrec arg))
        go i prec (Fix body) = (i, maybeParens (prec >= appPrec) $
          pretty "fix" <+> snd (go i initPrec body))
        go i prec (Cond c e1 e2) =
          (i, maybeParens (prec >= ifPrec) $ fillSep
            [ pretty "if"   <+> snd (go i initPrec c)
            , pretty "then" <+> snd (go i initPrec e1)
            , pretty "else" <+> snd (go i initPrec e2)
            ])
        go i prec (PrimBinOp e1 op e2) = (i, maybeParens (prec >= binOpPrec op)
          $ snd (go i (binOpLeftPrec op) e1)
            <+> pretty op
            <+> snd (go i (binOpRightPrec op) e2))
        go i prec (PrimOp op arg) = (i, maybeParens (prec >= opPrec op) $
          pretty op <> snd (go i (opPrecArg op) arg))
        go i _ (Pair f s) = (i, sGuillemetsOut $
          snd (go i initPrec f) <> comma <> snd (go i initPrec s))
        go i prec (LeftE l) = (i, maybeParens (prec >= leftPrec) $
          pretty "Left" <+> snd (go i initPrec l))
        go i prec (RightE r) = (i, maybeParens (prec >= rightPrec) $
          pretty "Right" <+> snd (go i initPrec r))
        go i prec (Case v l r) = (i, maybeParens (prec >= casePrec) $
          vsep [ pretty "case" <+> snd (go i initPrec v) <+> pretty "of"
               , pretty "Left ->" <+> snd (go i initPrec l)
               , pretty "Right ->" <+> snd (go i initPrec r)
               ])

-------------------------------------------------------------------------------
-- * Helper functions
-------------------------------------------------------------------------------

-- AST env a -> a
expType :: SingI a => AST VNil a -> SLType a
expType _ = sing

-- |Defines lambda with type argument passed implicitly
lambda :: SingI a => AST (a :> env) b -> AST env (LArrow a b)
lambda = Lambda sing

-- |Helper function that defines let expressions
letE :: SingI a => AST env a -> AST (a :> env) b -> AST env b
letE e1 e2 = App (Lambda sing e2) e1

-- |Helper function that defines recursive let expressions
letrecE :: SingI a => AST (a :> env) a -> AST (a :> env) b -> AST env b
letrecE e1 e2 = App (Lambda sing e2) (Fix (Lambda sing e1))
