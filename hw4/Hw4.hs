module Hw4 where

import Control.Applicative
import Data.Char  
import Parser   
import Hw3
import Data.List

space :: Parser ()
space = many (sat isSpace) >> return ()

var :: Parser Expr
var = do x <- sat isAlphaNum
         xs <- many alphaNum
         return (Var (x:xs))

app :: Parser Expr
app = do char '('
         e <- choiseExpr
         space
         e' <- choiseExpr >>= \e'' -> return e''
         char ')'
         return (App e e')


lambda :: Parser Expr
lambda = do char '('
            char '\\'
            x <- some alphaNum
            char '.'
            e <- choiseExpr >>= \e' -> return e'
            char ')'
            return (Lambda x e)
            
choiseExpr :: Parser Expr
choiseExpr = lambda <|> app <|> var 


element :: Parser a -> Parser a
element el = do space
                x <- el
                space
                return x

prg :: Parser Expr
prg = element choiseExpr

readPrg :: String -> Maybe Expr
readPrg str = result where pre_res = parse prg str
                           result | Just "" == (snd <$> pre_res) = fst <$> pre_res
                                  | otherwise = Nothing