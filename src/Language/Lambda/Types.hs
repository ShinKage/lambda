{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-------------------------------------------------------------------------------
-- |
-- Module      : Language.Lambda.Types
-- Description : Lambda types
-- Copyright   : (c) Giuseppe Lomurno, 2018
-- License     : MIT
-- Maintainer  : Giuseppe Lomurno <lomurno.giuseppe97@gmail.com>
-- Stability   : experimental
-- Portability : non-portable
--
-------------------------------------------------------------------------------

module Language.Lambda.Types
  ( LType(..)
  , Op(..)
  , BinOp(..)
  , SLType(..)
  ) where

import Data.Kind
import Data.Text.Prettyprint.Doc

import Language.Lambda.Data.Singletons

-------------------------------------------------------------------------------
-- * Types
-------------------------------------------------------------------------------

-- | Types of the Lambda language
data LType = LInt
           | LBool
           | LUnit
           | LVoid
           | LFun    LType LType
           | LPair   LType LType
           | LEither LType LType
  deriving Show

-- | Primitive unary operations, indexed by argument and result type.
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

-- | Primitive binary operations, indexed by argument and result type.
-- They must have the same type for both arguments.
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

-------------------------------------------------------------------------------
-- * Singletons
-------------------------------------------------------------------------------

-- | Singletons for 'LType'
data SLType :: LType -> Type where
  SLInt     :: SLType LInt
  SLBool    :: SLType LBool
  SLUnit    :: SLType LUnit
  SLVoid    :: SLType LVoid
  SLFun     :: SLType arg -> SLType res -> SLType (LFun arg res)
  SLPair    :: SLType f   -> SLType s   -> SLType (LPair f s)
  SLEither  :: SLType l   -> SLType r   -> SLType (LEither l r)

deriving instance Show (SLType t)

instance Pretty (SLType t) where
  pretty SLInt           = pretty "int"
  pretty SLBool          = pretty "bool"
  pretty SLUnit          = pretty "unit"
  pretty SLVoid          = pretty "void"
  pretty (SLFun arg res) = pretty arg <+> pretty "->" <+> pretty res
  pretty (SLPair f s)    = pretty f   <+> pretty '×'  <+> pretty s
  pretty (SLEither l r)  = pretty l   <+> pretty '⊕'  <+> pretty r

instance SingKind LType where
  type Sing = SLType

  fromSing SLInt           = LInt
  fromSing SLBool          = LBool
  fromSing SLUnit          = LUnit
  fromSing SLVoid          = LVoid
  fromSing (SLFun arg res) = LFun    (fromSing arg) (fromSing res)
  fromSing (SLPair f s)    = LPair   (fromSing f)   (fromSing s)
  fromSing (SLEither l r)  = LEither (fromSing l)   (fromSing r)

instance SingI LInt where
  sing = SLInt

instance SingI LBool where
  sing = SLBool

instance SingI LUnit where
  sing = SLUnit

instance SingI LVoid where
  sing = SLVoid

instance (SingI arg, SingI res) => SingI (LFun arg res) where
  sing = SLFun sing sing

instance (SingI f, SingI s) => SingI (LPair f s) where
  sing = SLPair sing sing

instance (SingI l, SingI r) => SingI (LEither l r) where
  sing = SLEither sing sing
