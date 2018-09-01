{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-------------------------------------------------------------------------------
-- |
-- Module      : Language.Lambda.Eval
-- Description : Evaluation and stepping functions
-- Copyright   : (c) Giuseppe Lomurno, 2018
-- License     : MIT
-- Maintainer  : Giuseppe Lomurno <lomurno.giuseppe97@gmail.com>
-- Stability   : experimental
-- Portability : non-portable
--
-------------------------------------------------------------------------------

module Language.Lambda.Eval
  ( Concrete
  , Step(..)
  , eval
  , step
  , stepDescent
  ) where

import Data.Kind
import Data.Text.Prettyprint.Doc

import Language.Lambda.AST
import Language.Lambda.Types
import Language.Lambda.Data.Vec

-- | Convert Lambda types to Haskell types
type family Concrete (t :: LType) :: Type where
  Concrete LInt           = Int
  Concrete LBool          = Bool
  Concrete LUnit          = ()
  Concrete (LArrow a b)   = AST VNil a -> AST VNil b
  Concrete (LProduct a b) = (Concrete a, Concrete b)
  Concrete (LSum a b)     = Either (Concrete a) (Concrete b)

-------------------------------------------------------------------------------
-- * Big-step evaluation
-------------------------------------------------------------------------------

-- | Evaluation function, convert a Lambda expression to the output
-- Haskell type
eval :: AST VNil a -> Concrete a
eval (IntE i)             = i
eval (BoolE b)            = b
eval UnitE                = ()
eval (Lambda _ body)      = \arg -> subst arg body
eval (Var v)              = case v of {}
eval (App body arg)       = eval (eval body arg)
eval (Fix body)           = eval $ unfix body (eval body)
eval (Cond c e1 e2)       = if eval c then eval e1 else eval e2
eval (PrimBinOp e1 op e2) = evalBinOp op (eval e1) (eval e2)
eval (PrimOp op arg)      = evalOp op (eval arg)
eval (Pair f s)           = (eval f, eval s)
eval (LeftE l)            = Left $ eval l
eval (RightE r)           = Right $ eval r
eval (Case (LeftE e) f _) = eval (App f e)
eval (Case (RightE e) _ f) = eval (App f e)
eval (Case e lfun rfun) = case step e of
  StepAST (LeftE l) -> eval (App lfun l)
  StepAST (RightE r) -> eval (App rfun r)
  StepAST e' -> eval (Case e' lfun rfun)
  _ -> undefined

evalBinOp :: BinOp a b -> Concrete a -> Concrete a -> Concrete b
evalBinOp PrimAdd    = (+)
evalBinOp PrimSub    = (-)
evalBinOp PrimMul    = (*)
evalBinOp PrimDiv    = div
evalBinOp PrimIntEq  = (==)
evalBinOp PrimBoolEq = (==)
evalBinOp PrimAnd    = (&&)
evalBinOp PrimOr     = (||)

evalOp :: Op a b -> Concrete a -> Concrete b
evalOp PrimNeg = negate
evalOp PrimNot = not
evalOp PrimFst = fst
evalOp PrimSnd = snd

-------------------------------------------------------------------------------
-- * Small-step evaluation
-------------------------------------------------------------------------------

-- | Stepped evaluation can lead to expression substitution or evaluation.
-- This type contains both cases
data Step :: LType -> Type where
  StepAST   :: AST VNil a -> Step a
  StepValue :: AST VNil a -> Concrete a -> Step a

instance Pretty (Step a) where
  pretty (StepAST e) = pretty e
  pretty (StepValue e _) = pretty e

-- | Stepping function for closed expressions.
step :: AST VNil a -> Step a
step e@(IntE i) = StepValue e i
step e@(BoolE b) = StepValue e b
step UnitE = StepValue UnitE ()
step e@(Lambda _ body) = StepValue e $ \arg -> subst arg body
step (Var v) = case v of {}
step (App body arg) = case step body of
  StepAST body'         -> StepAST (App body' arg)
  StepValue ebody body' -> case step arg of
    StepAST arg'     -> StepAST (App ebody arg')
    StepValue earg _ -> StepAST (body' earg)
step (Fix body) = case step body of
  StepAST body'         -> StepAST (Fix body')
  StepValue ebody body' -> StepAST (unfix ebody body')
step (Cond c e1 e2) = case step c of
  StepAST c'     -> StepAST (Cond c' e1 e2)
  StepValue _ c' -> StepAST (if c' then e1 else e2)
step (PrimBinOp e1 op e2) = case step e1 of
  StepAST e1'       -> StepAST (PrimBinOp e1' op e2)
  StepValue ee1 e1' -> case step e2 of
    StepAST e2'     -> StepAST (PrimBinOp ee1 op e2')
    StepValue _ e2' -> stepBinOp op e1' e2'
step (PrimOp op arg) = case step arg of
  StepAST arg'        -> StepAST (PrimOp op arg')
  StepValue earg arg' -> stepOp op earg arg'
step (Pair f s) = case step f of
  StepAST f'      -> StepAST (Pair f' s)
  StepValue ef f' -> case step s of
    StepAST s'      -> StepAST (Pair ef s')
    StepValue es s' -> StepValue (Pair ef es) (f', s')
step (LeftE l) = case step l of
  StepAST l'      -> StepAST (LeftE l')
  StepValue el l' -> StepValue (LeftE el) (Left l')
step (RightE r) = case step r of
  StepAST r'      -> StepAST (RightE r')
  StepValue er r' -> StepValue (RightE er) (Right r')
step (Case c lf rf) = case step c of
  StepAST c' -> StepAST (Case c' lf rf)
  StepValue ec _ -> case step lf of
    StepAST lf' -> StepAST (Case ec lf' rf)
    StepValue el _ -> case step rf of
      StepAST rf' -> StepAST (Case ec el rf')
      StepValue er _ -> case ec of
        LeftE l -> StepAST (App el l)
        RightE r -> StepAST (App er r)
        _ -> undefined

-- | Fully evaluates a closed expression saving in a list all the intermediates
-- expressions or values.
stepDescent :: AST VNil a -> [AST VNil a]
stepDescent e = e : case step e of
                      StepAST e'     -> stepDescent e'
                      StepValue e' _ -> [e']

stepBinOp :: BinOp a b -> Concrete a -> Concrete a -> Step b
stepBinOp PrimAdd a b    = StepValue (IntE (a + b)) (a + b)
stepBinOp PrimSub a b    = StepValue (IntE (a - b)) (a - b)
stepBinOp PrimMul a b    = StepValue (IntE (a * b)) (a * b)
stepBinOp PrimDiv a b    = StepValue (IntE (a `div` b)) (a `div` b)
stepBinOp PrimIntEq a b  = StepValue (BoolE (a == b)) (a == b)
stepBinOp PrimBoolEq a b = StepValue (BoolE (a == b)) (a == b)
stepBinOp PrimAnd a b    = StepValue (BoolE (a && b)) (a && b)
stepBinOp PrimOr a b     = StepValue (BoolE (a || b)) (a || b)

stepOp :: Op a b -> AST VNil a -> Concrete a -> Step b
stepOp PrimNeg _ a = StepValue (IntE (negate a)) (negate a)
stepOp PrimNot _ a = StepValue (BoolE (not a)) (not a)
stepOp PrimFst (Pair a _) arg = StepValue a (fst arg)
stepOp PrimSnd (Pair _ a) arg = StepValue a (snd arg)
stepOp _ _ _ = undefined

-------------------------------------------------------------------------------
-- * Helper functions
-------------------------------------------------------------------------------

unfix :: AST VNil (LArrow a a) -> Concrete (LArrow a a) -> AST VNil a
unfix lam v = v $ Fix lam

subst :: forall env sub res. AST env sub -> AST (sub :> env) res -> AST env res
subst e = go LZero
  where go :: Length (locals :: Vec LType n)
           -> AST (locals +++ sub :> env) t
           -> AST (locals +++ env) t
        go _   (IntE n)             = IntE n
        go _   (BoolE b)            = BoolE b
        go _   UnitE                = UnitE
        go len (Lambda ty body)     = Lambda ty (go (LSucc len) body)
        go len (Var v)              = substVar len v
        go len (App body arg)       = App (go len body) (go len arg)
        go len (Fix body)           = Fix (go len body)
        go len (Cond c e1 e2)       = Cond (go len c) (go len e1) (go len e2)
        go len (PrimBinOp e1 op e2) = PrimBinOp (go len e1) op (go len e2)
        go len (PrimOp op arg)      = PrimOp op (go len arg)
        go len (Pair f s)           = Pair (go len f) (go len s)
        go len (LeftE l)            = LeftE (go len l)
        go len (RightE l)           = RightE (go len l)
        go len (Case c l r)         = Case (go len c) (go len l) (go len r)

        substVar :: Length (locals :: Vec LType n)
                 -> Elem (locals +++ sub :> env) t
                 -> AST (locals +++ env) t
        substVar LZero       EZero     = e
        substVar LZero       (ESucc v) = Var v
        substVar (LSucc _)   EZero     = Var EZero
        substVar (LSucc len) (ESucc v) = shift $ substVar len v

shift :: AST env ty -> AST (t :> env) ty
shift = shifts $ LSucc LZero

shifts :: forall prefix env ty. Length prefix
       -> AST env ty
       -> AST (prefix +++ env) ty
shifts prefix = go LZero
  where go :: Length (locals :: Vec LType n)
           -> AST (locals +++ env) t
           -> AST (locals +++ prefix +++ env) t
        go _   (IntE n)               = IntE n
        go _   (BoolE b)              = BoolE b
        go _   UnitE                  = UnitE
        go len (Lambda ty body)       = Lambda ty (go (LSucc len) body)
        go len (Var v)                = Var (shiftsVar len v)
        go len (App body arg)         = App (go len body) (go len arg)
        go len (Fix body)             = Fix (go len body)
        go len (Cond c e1 e2)         = Cond (go len c) (go len e1) (go len e2)
        go len (PrimBinOp lhs op rhs) = PrimBinOp (go len lhs) op (go len rhs)
        go len (PrimOp op arg)        = PrimOp op (go len arg)
        go len (Pair f s)             = Pair (go len f) (go len s)
        go len (LeftE l)              = LeftE (go len l)
        go len (RightE l)             = RightE (go len l)
        go len (Case c l r)         = Case (go len c) (go len l) (go len r)

        shiftsVar :: Length (locals :: Vec LType n)
                  -> Elem (locals +++ env) t
                  -> Elem (locals +++ prefix +++ env) t
        shiftsVar LZero     v         = weakenElem prefix v
        shiftsVar (LSucc _) EZero     = EZero
        shiftsVar (LSucc l) (ESucc e) = ESucc $ shiftsVar l e
