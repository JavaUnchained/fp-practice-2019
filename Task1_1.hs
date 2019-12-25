module Task1_1 where

{-
  Задание 1.1
  Необходимо реализовать все операции, описанные в данном файле
-}

import Todo(todo)

data BinOperators = Plus | Diff | Mult deriving (Show, Eq)

data Term = IntConstant{ intValue :: Int }           -- числовая константа
            | Variable{ varName :: String }          -- переменная
            | BinaryTerm{ bo :: BinOperators, lhv :: Term, rhv :: Term } -- бинарная операция
            deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) l r = BinaryTerm Plus l r 

(|-|) :: Term -> Term -> Term
(|-|) l r = BinaryTerm Diff l r

(|*|) :: Term -> Term -> Term
(|*|) l r = BinaryTerm Mult l r

infixl 6 |+|, |-|
infixl 7 |*|

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement expression = replace expression
    where
      replace (Variable var) | var == varName = replacement
                             | otherwise      = expression
      replace (BinaryTerm bo l r) = BinaryTerm bo (replace l) (replace r)
      replace _ = expression

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate expression = case expression of
      BinaryTerm bo l r ->
        case (bo, evaluate(l), evaluate(r) ) of
          (Plus, IntConstant l, IntConstant r) -> IntConstant $ l + r
          (Plus, IntConstant 0, r)             -> r
          (Plus, l, IntConstant 0)             -> l
          (Diff, IntConstant l, IntConstant r) -> IntConstant $ l - r
          (Diff, l, IntConstant 0)           -> l
          (Mult, IntConstant l, IntConstant r) -> IntConstant $ l * r 
          (Mult, IntConstant 1, r)             -> r
          (Mult, IntConstant 0, r)             -> IntConstant 0
          (Mult, l, IntConstant 0)             -> IntConstant 0
          (Mult, l, IntConstant 1)             -> l
          _ -> BinaryTerm bo l r
      _ -> expression
