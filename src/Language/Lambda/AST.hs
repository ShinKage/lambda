{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Language.Lambda.AST where

import Data.Kind
import Data.Text.Prettyprint.Doc

import Language.Lambda.Types
--import Language.Lambda.Data.Nat
import Language.Lambda.Data.Singletons
import Language.Lambda.Data.Vec

data AST :: forall n. Vec LType n -> LType -> Type where
  IntE      :: Int -> AST env LInt
  BoolE     :: Bool -> AST env LBool
  Lambda    :: SLType arg -> AST (arg :> env) res -> AST env (LFun arg res)
  Var       :: Elem env ty -> AST env ty
  App       :: AST env (LFun arg res) -> AST env arg -> AST env res
  Fix       :: AST env (LFun ty ty) -> AST env ty
  Cond      :: AST env LBool -> AST env ty -> AST env ty -> AST env ty
  PrimBinOp :: AST env arg -> BinOp arg res -> AST env arg -> AST env res
  PrimOp    :: Op arg res -> AST env arg -> AST env res
  Pair      :: AST env ty1 -> AST env ty2 -> AST env (LPair ty1 ty2)

deriving instance Show (AST env ty)

instance Pretty (AST VNil ty) where
  pretty = prettyAST_

letE :: SingI arg => AST env arg -> AST (arg :> env) res -> AST env res
letE e1 e2 = App (Lambda sing e2) e1

letrecE :: SingI arg => AST (arg :> env) arg -> AST (arg :> env) res -> AST env res
letrecE e1 e2 = App (Lambda sing e2) (Fix (Lambda sing e1))

{-prettyAST :: forall (n :: Nat) (ty :: LType) (env :: Vec LType n) ann. SingI n => AST env ty -> Doc ann
prettyAST (IntE n) = pretty n
prettyAST (BoolE b) = pretty b
{-prettyAST (Lambda ty body) = parens $ fillSep
  [ pretty 'λ' <> pretty '#' <> pretty ':' <> pretty ty <> pretty '.'
  , pretty body
  ]-}
prettyAST (Lambda ty body) = prettyLambda ty body
prettyAST (Var v) = pretty '#' <> pretty (elemToInt v)
prettyAST (App body arg) = parens $ prettyAST body <+> prettyAST arg
prettyAST (Fix body) = pretty "fix" <+> prettyAST body
prettyAST (Cond c e1 e2) = fillSep [ pretty "if" <+> prettyAST c
                                   , pretty "then" <+> prettyAST e1
                                   , pretty "else" <+> prettyAST e2
                                   ]
prettyAST (PrimBinOp e1 op e2) = parens $ prettyAST e1 <+> pretty op <+> prettyAST e2
prettyAST (PrimOp op arg) = parens $ pretty op <> prettyAST arg

prettyLambda :: forall (n :: Nat) (arg :: LType) (env :: Vec LType n) res ann
              . SingI n => SLType arg -> AST env res -> Doc ann
prettyLambda ty body = parens $ fillSep
  [ pretty 'λ' <> pretty '#' <> pretty (snatToInt (sing :: SNat n) - 1)
      <> pretty ':' <> pretty ty <> pretty '.'
  , prettyAST body
  ]-}

prettyAST_ :: AST env ty -> Doc ann
prettyAST_ e = snd (go 0 e)
  where
    go :: Int -> AST env ty -> (Int, Doc ann)
    go i (IntE n) = (i, pretty n)
    go i (BoolE b) = (i, pretty b)
    go i (Lambda ty body) = case go i body of
      (j, doc) -> (i + 1, parens $ fillSep
        [ pretty 'λ' <> pretty '#' <> pretty j <> pretty ':' <> pretty ty
            <> pretty '.'
        , doc
        ])
    go i (Var v) = (i, pretty '#' <> pretty (elemToInt v))
    go i (App body arg) = (i, parens $ snd (go i body) <+> snd (go i arg))
    go i (Fix body) = (i, pretty "fix" <+> snd (go i body))
    go i (Cond c e1 e2) =
      (i, fillSep [ pretty "if" <+> snd (go i c)
                  , pretty "then" <+> snd (go i e1)
                  , pretty "else" <+> snd (go i e2)
                  ])
    go i (PrimBinOp e1 op e2) = (i, parens $ snd (go i e1) <+> pretty op <+> snd (go i e2))
    go i (PrimOp op arg) = (i, parens $ pretty op <> snd (go i arg))
    go i (Pair f s) = (i, angles $ snd (go i f) <> comma <> snd (go i s))
