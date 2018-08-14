{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Lambda.Data.Singletons where

import Data.Kind

class SingKind k where
  type Sing :: k -> Type

  fromSing :: Sing (a :: k) -> k

class SingI (a :: k) where
  sing :: Sing a
