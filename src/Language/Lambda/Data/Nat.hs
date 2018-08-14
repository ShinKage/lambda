{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Language.Lambda.Data.Nat where

import Data.Kind

import Language.Lambda.Data.Singletons

data Nat = Zero | Succ Nat
  deriving Show

type family n + m where
  Zero   + m = m
  Succ n + m = Succ (n + m)

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

snatToInt :: SNat n -> Int
snatToInt SZero = 0
snatToInt (SSucc n) = 1 + snatToInt n
