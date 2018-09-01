{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}

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
           | LBottom
           | LProduct LType LType
           | LSum     LType LType
           | LArrow   LType LType
  deriving Show

-- | Primitive unary operations, indexed by argument and result type.
data Op :: LType -> LType -> Type where
  PrimNeg :: Op LInt            LInt
  PrimNot :: Op LBool           LBool
  PrimFst :: Op (LProduct a b)  a
  PrimSnd :: Op (LProduct a b)  b

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
  SLBottom  :: SLType LBottom
  SLArrow   :: SLType a -> SLType b -> SLType (LArrow a b)
  SLProduct :: SLType a -> SLType b -> SLType (LProduct a b)
  SLSum     :: SLType a -> SLType b -> SLType (LSum a b)

deriving instance Show (SLType t)

instance Pretty (SLType t) where
  pretty SLInt           = pretty "int"
  pretty SLBool          = pretty "bool"
  pretty SLUnit          = pretty "()"
  pretty SLBottom        = pretty "⊥"
  pretty (SLArrow a b)   = pretty a <+> pretty "->" <+> pretty b
  pretty (SLProduct a b) = pretty a <+> pretty "×"  <+> pretty b
  pretty (SLSum a b)     = pretty a <+> pretty "⊕"  <+> pretty b

instance SingKind LType where
  type Sing = SLType

  fromSing SLInt           = LInt
  fromSing SLBool          = LBool
  fromSing SLUnit          = LUnit
  fromSing SLBottom        = LBottom
  fromSing (SLArrow a b)   = LArrow   (fromSing a) (fromSing b)
  fromSing (SLProduct a b) = LProduct (fromSing a) (fromSing b)
  fromSing (SLSum a b)     = LSum     (fromSing a) (fromSing b)

instance SingI LInt where
  sing = SLInt

instance SingI LBool where
  sing = SLBool

instance SingI LUnit where
  sing = SLUnit

instance SingI LBottom where
  sing = SLBottom

instance (SingI a, SingI b) => SingI (LArrow a b) where
  sing = SLArrow sing sing

instance (SingI a, SingI b) => SingI (LProduct a b) where
  sing = SLProduct sing sing

instance (SingI a, SingI b) => SingI (LSum a b) where
  sing = SLSum sing sing
