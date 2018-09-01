{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Main where

import Data.List (intersperse)
import Data.Text.Prettyprint.Doc (line, pretty, vsep)

import Language.Lambda.AST
import Language.Lambda.Eval
import Language.Lambda.Types
import Language.Lambda.Data.Singletons
import Language.Lambda.Data.Vec

main :: IO ()
main = print . vsep . intersperse line . fmap pretty . stepDescent $ App factorial (IntE 5)

factorial :: AST VNil (LArrow LInt LInt)
factorial = Fix fact
  where fact = Lambda sing (Lambda sing
                (Cond
                  (PrimBinOp (Var EZero) PrimIntEq (IntE 0))
                  (IntE 1)
                  (PrimBinOp (Var EZero) PrimMul
                    (App
                      (Var (ESucc EZero))
                      (PrimBinOp (Var EZero) PrimSub (IntE 1))
                    )
                  )
                ))

fibonacci :: AST VNil (LArrow LInt LInt)
fibonacci = Fix fib
  where fib = Lambda sing (Lambda sing
                (Cond
                  (PrimBinOp
                    (PrimBinOp (Var EZero) PrimIntEq (IntE 0))
                    PrimOr
                    (PrimBinOp (Var EZero) PrimIntEq (IntE 1))
                  )
                  (IntE 1)
                  (PrimBinOp
                    (App
                      (Var (ESucc EZero))
                      (PrimBinOp (Var EZero) PrimSub (IntE 1))
                    )
                    PrimAdd
                    (App
                      (Var (ESucc EZero))
                      (PrimBinOp (Var EZero) PrimSub (IntE 2))
                    )
                  )
                ))

negFst :: SingI s => AST VNil (LArrow (LProduct LInt s) LInt)
negFst = Lambda sing (PrimOp PrimNeg (PrimOp PrimFst (Var EZero)))

test :: AST VNil LInt
test = letE (IntE 5) (PrimBinOp (Var EZero) PrimMul (IntE 2))


type LMaybe a = LSum a LUnit
type LReader r a = LArrow r a
type LWriter w a = LProduct a w
type LState s a = LReader s (LWriter s a)

just :: SingI a => AST VNil a -> AST VNil (LMaybe a)
just = LeftE

nothing :: SingI a => AST VNil (LMaybe a)
nothing = RightE UnitE
