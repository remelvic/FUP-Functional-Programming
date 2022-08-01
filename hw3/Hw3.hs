module Hw3 where 

import qualified Data.Set as Set

type Symbol = String
data Expr = Var Symbol | App Expr Expr | Lambda Symbol Expr deriving Eq

instance Show Expr where
    show (Var a) = a
    show (App a b) = "(" ++ show a ++ " " ++ show b ++ ")"
    show (Lambda s e) = "(\\" ++ s ++ "." ++ show e ++ ")"

-- insirace: https://cw.fel.cvut.cz/wiki/courses/fup/tutorials/lab_7_-_lambda_calculus

symbols :: [Symbol] 
symbols = ["a" ++ show x | x <- [0..]]

-- -- (\x .(\y . x)) -> x isn't free
-- -- (\y. x) -> x is free

-- -- 1) x[x := N] return N if x == N
-- -- 2) y[x := N] return y if x != y
-- -- 3) (PQ) [x := N] return P[x := N]  Q[x := N] 
-- -- 4) (\y.P) [x := N] return (\y. P[x := N]) 
-- -- 5) (\x.P) [x := N] return (\x.P) 
-- -- 6) (\y.P) [x := N] return (\z.P[y := z][x := N]) if y != x and y free in N and z is fresh 

isASymbol :: Symbol -> Bool
isASymbol x | x == "a" = True
            | otherwise = False

-- -- 1)ALPHA conversion 
-- -- 2) BETHA reduction

-- elementExists :: Symbol -> [Symbol] -> Bool
-- elementExists _ []  = False
-- elementExists a (x:xs) | x == a = True
--                        | otherwise = elementExists a xs

-- addSet :: Symbol -> [Symbol]-> [Symbol]
-- addSet x [] = [x]
-- addSet a (x:xs) = x : addSet a xs 

-- removeSet :: Symbol -> [Symbol] -> [Symbol]
-- removeSet _ [] = []
-- removeSet x (y:ys) | x == y = removeSet x ys
--                    | otherwise = y : removeSet x ys


getFreeVarLambda :: Expr -> Symbol
getFreeVarLambda (Var x) = x
getFreeVarLambda (Lambda x expr) = getFreeVarLambda expr


getFreeVar :: Expr -> Expr -> Bool
getFreeVar (Var var) (Var new) | var == new = True
                               | otherwise = False

getFreeVar (App p q) (Var new) = getFreeVar p (Var new) || getFreeVar q (Var new)
getFreeVar (Lambda x expr) (Var new) | x == new = False
                                     | otherwise = getFreeVar expr (Var new)

checkVar :: Expr -> Expr -> Expr -> Int -> Expr 
checkVar (Var x) expr q count  | (getFreeVar expr (Var x)) = Var (symbols !! count)
                               | otherwise = Var x

checkVar (App p q) expr z count = (App (checkVar p expr z count) (checkVar q expr z count))

checkVar (Lambda x expr) var q count | (getFreeVar expr (Var x)) = let (Var new_var) = (Var (symbols !! count))
                                                                              in (Lambda new_var (substitute (substitute (checkVar expr (Var x) q count) (show expr) (Var x) count) (show new_var) q (count+1)))
                                     | otherwise = (Lambda x (substitute expr (show var) q count))


substitute :: Expr -> Symbol -> Expr -> Int -> Expr 
substitute (Var expr) var val count | expr == var = val
                                    | otherwise = (Var expr)

substitute (Lambda x expr) var q count | var == x = Lambda x expr
                                       | otherwise = (checkVar (Lambda x expr) (Var var) q count)
substitute (App p l) var q count = (App (substitute p (show var) q count) (substitute l (show var) q count))

cbn :: Expr -> Expr
cbn (Var x) = Var x
cbn (Lambda x expr) = (Lambda x expr)
cbn (App p q) = case cbn p of 
                    Lambda x expr -> cbn (substitute expr x q 0)
                    p'            -> App p' q

nor :: Expr -> Expr
nor (Var x) = Var x
nor (Lambda x expr) = (Lambda x (nor expr))
nor (App p q) = case cbn p of 
                     Lambda x expr -> nor (substitute expr x q 0) 
                     p'            -> let p2 = (nor p')
                                      in App p2 (nor q)

eval :: Expr -> Expr
eval lam = (nor lam)
-- eval (App (Lambda "x" (Lambda "y" (Lambda "z" (App (App (Var "x") (Var "y")) (Var "z"))))) (App (Var "y") (Var "z")))