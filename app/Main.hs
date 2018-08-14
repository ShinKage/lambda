{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Main where

import Language.Lambda.AST
import Language.Lambda.Eval
import Language.Lambda.Types
import Language.Lambda.Data.Singletons
import Language.Lambda.Data.Vec

main :: IO ()
main = putStrLn . show . eval $ App factorial (IntE 5)

factorial :: AST VNil (LFun LInt LInt)
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

fibonacci :: AST VNil (LFun LInt LInt)
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

negFst :: SingI s => AST VNil (LFun (LPair LInt s) LInt)
negFst = Lambda sing (PrimOp PrimNeg (PrimOp PrimFst (Var EZero)))

test :: AST VNil LInt
test = letE (IntE 5) (PrimBinOp (Var EZero) PrimMul (IntE 2))
