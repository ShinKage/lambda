{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Language.Lambda.Eval where

import Data.Kind

import Language.Lambda.AST
import Language.Lambda.Types
import Language.Lambda.Data.Vec

type family Concrete (t :: LType) :: Type where
  Concrete LInt = Int
  Concrete LBool = Bool
  Concrete (LFun arg res) = AST VNil arg -> AST VNil res

eval :: AST VNil a -> Concrete a
eval (IntE i) = i
eval (BoolE b) = b
eval (Lambda _ body) = \arg -> subst arg body
eval (Var v) = case v of {} -- impossible in a well formed expression
eval (App body arg) = eval (eval body $ arg)
eval (Fix body) = eval $ unfix body (eval body)
eval (Cond c e1 e2) = if eval c then eval e1 else eval e2
eval (PrimBinOp e1 op e2) = evalBinOp op (eval e1) (eval e2)
eval (PrimOp op arg) = evalOp op (eval arg)

unfix :: AST VNil (LFun a a) -> Concrete (LFun a a) -> AST VNil a
unfix lam v = v $ Fix lam

evalBinOp :: BinOp a b -> Concrete a -> Concrete a -> Concrete b
evalBinOp PrimAdd = (+)
evalBinOp PrimSub = (-)
evalBinOp PrimMul = (*)
evalBinOp PrimDiv = div
evalBinOp PrimIntEq = (==)
evalBinOp PrimBoolEq = (==)
evalBinOp PrimAnd = (&&)
evalBinOp PrimOr = (||)

evalOp :: Op a b -> Concrete a -> Concrete b
evalOp PrimNeg = negate
evalOp PrimNot = not

subst :: forall env sub res. AST env sub -> AST (sub :> env) res -> AST env res
subst e = go LZero
  where
    go :: Length (locals :: Vec LType n)
       -> AST (locals +++ sub :> env) t
       -> AST (locals +++ env) t
    go _   (IntE n)             = IntE n -- No subst necessary
    go _   (BoolE b)            = BoolE b -- No subst necessary
    go len (Lambda ty body)     = Lambda ty (go (LSucc len) body) -- Check the body, keep the count of the index
    go len (Var v)              = substVar len v -- Check if is the correct variable
    go len (App body arg)       = App (go len body) (go len arg) -- Check body and argument
    go len (Fix body)           = Fix (go len body) -- Check body
    go len (Cond c e1 e2)       = Cond (go len c) (go len e1) (go len e2) -- Check clause and exprs
    go len (PrimBinOp e1 op e2) = PrimBinOp (go len e1) op (go len e2) -- Check exprs
    go len (PrimOp op arg)      = PrimOp op (go len arg) -- Check arg

    substVar :: Length (locals :: Vec LType n)
             -> Elem (locals +++ sub :> env) t
             -> AST (locals +++ env) t
    substVar LZero       EZero     = e -- Same index, apply subst
    substVar LZero       (ESucc v) = Var v -- Index too high, keep var
    substVar (LSucc _)   EZero     = Var EZero  -- Index too low
    substVar (LSucc len) (ESucc v) = shift $ substVar len v -- Need to shift to compensate the subst

-- Helper, shift only one place
shift :: AST env ty -> AST (t :> env) ty
shift = shifts $ LSucc LZero

-- Explicit forall for scoping
shifts :: forall prefix env ty. Length prefix
       -> AST env ty
       -> AST (prefix +++ env) ty
shifts prefix = go LZero
  where
    go :: Length (locals :: Vec LType n)
       -> AST (locals +++ env) t
       -> AST (locals +++ prefix +++ env) t
    go _   (IntE n)               = IntE n
    go _   (BoolE b)              = BoolE b
    go len (Lambda ty body)       = Lambda ty (go (LSucc len) body)
    go len (Var v)                = Var (shiftsVar len v) -- shifts only apply to vars
    go len (App body arg)         = App (go len body) (go len arg)
    go len (Fix body)             = Fix (go len body)
    go len (Cond c e1 e2)         = Cond (go len c) (go len e1) (go len e2)
    go len (PrimBinOp lhs op rhs) = PrimBinOp (go len lhs) op (go len rhs)
    go len (PrimOp op arg)        = PrimOp op (go len arg)

    shiftsVar :: Length (locals :: Vec LType n)
              -> Elem (locals +++ env) t
              -> Elem (locals +++ prefix +++ env) t
    shiftsVar LZero     v         = weakenElem prefix v
    shiftsVar (LSucc _) EZero     = EZero
    shiftsVar (LSucc l) (ESucc e) = ESucc $ shiftsVar l e
