{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Lambda.Data.Vec where

import Data.Kind
import Data.Type.Equality

import Language.Lambda.Data.Nat
import Language.Lambda.Data.Singletons

data Vec :: Type -> Nat -> Type where
  VNil :: Vec a Zero
  (:>) :: a -> Vec a n -> Vec a (Succ n)
infixr 5 :>

deriving instance Show a => Show (Vec a n)

data SVec :: forall (a :: Type) (n :: Nat). Vec a n -> Type where
  SVNil :: SVec VNil
  (:%>) :: Sing a -> Sing as -> SVec (a :> as)
infixr 5 :%>

deriving instance ShowSingVec v => Show (SVec v)

type family ShowSingVec (v :: Vec a n) :: Constraint where
  ShowSingVec VNil = ()
  ShowSingVec (x :> xs) = (Show (Sing x), ShowSingVec xs)

instance SingKind a => SingKind (Vec a n) where
  type Sing = SVec

  fromSing SVNil = VNil
  fromSing (x :%> xs) = fromSing x :> fromSing xs

instance SingI VNil where
  sing = SVNil

instance (SingI x, SingI xs) => SingI (x :> xs) where
  sing = sing :%> sing

data Elem :: forall a n. Vec a n -> a -> Type where
  EZero :: Elem (x :> xs) x
  ESucc :: Elem xs x -> Elem (y :> xs) x

deriving instance Show (Elem v a)

elemToInt :: Elem v x -> Int
elemToInt EZero = 0
elemToInt (ESucc e) = 1 + elemToInt e

eqElem :: Elem xs x1 -> Elem xs x2 -> Maybe (x1 :~: x2)
eqElem EZero EZero           = Just Refl
eqElem (ESucc e1) (ESucc e2) = eqElem e1 e2
eqElem _ _                   = Nothing

instance TestEquality (Elem xs) where
  testEquality = eqElem

data Length :: forall a n. Vec a n -> Type where
  LZero :: Length VNil
  LSucc :: Length xs -> Length (x :> xs)

deriving instance Show (Length v)

type family (v1 :: Vec a n) +++ (v2 :: Vec a m) :: Vec a (n + m) where
  (_ :: Vec a Zero) +++ v2 = v2
  (x :> xs)         +++ v2 = x :> (xs +++ v2)
infixr 5 +++

weakenElem :: Length prefix -> Elem xs x -> Elem (prefix +++ xs) x
weakenElem LZero e       = e
weakenElem (LSucc len) e = ESucc $ weakenElem len e

strengthenElem :: Length prefix -> Elem (prefix +++ xs) x -> Maybe (Elem xs x)
strengthenElem LZero e = Just e
strengthenElem (LSucc _) EZero = Nothing
strengthenElem (LSucc len) (ESucc e) = strengthenElem len e
