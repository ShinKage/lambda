{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-------------------------------------------------------------------------------
-- |
-- Module      : Language.Lambda.Data.Nat
-- Description : Peano natural numbers.
-- Copyright   : (c) Giuseppe Lomurno, 2018
-- License     : MIT
-- Maintainer  : Giuseppe Lomurno <lomurno.giuseppe97@gmail.com>
-- Stability   : experimental
-- Portability : non-portable
--
-------------------------------------------------------------------------------

module Language.Lambda.Data.Nat where

import Data.Kind

import Language.Lambda.Data.Singletons

-------------------------------------------------------------------------------
-- * Types
-------------------------------------------------------------------------------

-- | Inductive definition of Peano natural numbers.
data Nat = Zero | Succ Nat
  deriving Show

-- | Type-level natural numbers addition.
type family n + m where
  Zero   + m = m
  Succ n + m = Succ (n + m)

-------------------------------------------------------------------------------
-- * Singletons
-------------------------------------------------------------------------------

-- | Singletons for 'Nat'
data SNat :: Nat -> Type where
  SZero :: SNat Zero
  SSucc :: SNat n -> SNat (Succ n)

deriving instance Show (SNat n)

instance SingKind Nat where
  type Sing = SNat

  fromSing SZero = Zero
  fromSing (SSucc n) = Succ (fromSing n)

instance SingI Zero where
  sing = SZero

instance SingI n => SingI (Succ n) where
  sing = SSucc sing

-------------------------------------------------------------------------------
-- * Helper functions
-------------------------------------------------------------------------------

-- | Converts 'Nat' singleton instance to an 'Int'
snatToInt :: SNat n -> Int
snatToInt SZero = 0
snatToInt (SSucc n) = 1 + snatToInt n
