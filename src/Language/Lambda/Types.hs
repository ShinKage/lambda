{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Lambda.Types where

import Data.Kind
import Data.Text.Prettyprint.Doc

import Language.Lambda.Data.Singletons

data LType = LInt | LBool | LFun LType LType | LPair LType LType
  deriving Show

data Op :: LType -> LType -> Type where
  PrimNeg :: Op LInt        LInt
  PrimNot :: Op LBool       LBool
  PrimFst :: Op (LPair f s) f
  PrimSnd :: Op (LPair f s) s

deriving instance Show (Op arg res)

instance Pretty (Op arg res) where
  pretty PrimNeg = pretty '-'
  pretty PrimNot = pretty '¬'
  pretty PrimFst = pretty "fst "
  pretty PrimSnd = pretty "snd "

data BinOp :: LType -> LType -> Type where
  PrimAdd    :: BinOp LInt  LInt
  PrimSub    :: BinOp LInt  LInt
  PrimMul    :: BinOp LInt  LInt
  PrimDiv    :: BinOp LInt  LInt
  PrimIntEq  :: BinOp LInt  LBool
  PrimBoolEq :: BinOp LBool LBool
  PrimAnd    :: BinOp LBool LBool
  PrimOr     :: BinOp LBool LBool

deriving instance Show (BinOp arg res)

instance Pretty (BinOp arg res) where
  pretty PrimAdd    = pretty '+'
  pretty PrimSub    = pretty '-'
  pretty PrimMul    = pretty '•'
  pretty PrimDiv    = pretty '/'
  pretty PrimIntEq  = pretty '='
  pretty PrimBoolEq = pretty '='
  pretty PrimAnd    = pretty '∧'
  pretty PrimOr     = pretty '∨'

data SLType :: LType -> Type where
  SLInt     :: SLType LInt
  SLBool    :: SLType LBool
  SLFun     :: SLType arg -> SLType res -> SLType (LFun arg res)
  SLPair    :: SLType f   -> SLType s   -> SLType (LPair f s)

deriving instance Show (SLType t)

instance Pretty (SLType t) where
  pretty SLInt           = pretty "int"
  pretty SLBool          = pretty "boolean"
  pretty (SLFun arg res) = pretty arg <+> pretty "->" <+> pretty res
  pretty (SLPair f s)    = pretty f <+> pretty '×' <+> pretty s

instance SingKind LType where
  type Sing = SLType

  fromSing SLInt           = LInt
  fromSing SLBool          = LBool
  fromSing (SLFun arg res) = LFun (fromSing arg) (fromSing res)
  fromSing (SLPair f s)    = LPair (fromSing f) (fromSing s)

instance SingI LInt where
  sing = SLInt

instance SingI LBool where
  sing = SLBool

instance (SingI arg, SingI res) => SingI (LFun arg res) where
  sing = SLFun sing sing

instance (SingI f, SingI s) => SingI (LPair f s) where
  sing = SLPair sing sing
