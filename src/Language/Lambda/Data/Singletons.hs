{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}

-------------------------------------------------------------------------------
-- |
-- Module      : Language.Lambda.Data.Singletons
-- Description : Simple singletons.
-- Copyright   : (c) Giuseppe Lomurno, 2018
-- License     : MIT
-- Maintainer  : Giuseppe Lomurno <lomurno.giuseppe97@gmail.com>
-- Stability   : experimental
-- Portability : non-portable
--
-------------------------------------------------------------------------------

module Language.Lambda.Data.Singletons where

import Data.Kind

-- | Types with singletons.
class SingKind k where
  type Sing :: k -> Type

  -- | Convert singleton to type.
  fromSing :: Sing (a :: k) -> k

-- | Implicit singleton instance given type.
class SingI (a :: k) where
  sing :: Sing a
