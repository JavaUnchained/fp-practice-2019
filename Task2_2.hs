module Task2_2 where

{-
  Задание 2.2
  Необходимо реализовать функции foldl, foldr и unfoldr, а также все остальные функции
  в данном файле _на основе этих трёх_
-}

import Todo(todo)

import Prelude hiding (foldl, foldr, unfoldr, map, concatMap,
    filter, maxBy, minBy, reverse, sum, product, elem)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f i []     = i
foldl f i (l:ls) = foldl f (f i l) ls 

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f i []       = i
foldr f i (r : rs) = f r $ foldr f i rs

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr f i = unf (f i) where
  unf (Just (r, i')) = r : unfoldr f i'
  unf Nothing        = [] 

-- Сумма всех элементов списка (пример)
sum :: [Integer] -> Integer
sum lst = foldl (+) 0 lst

-- Переворот списка (Пример)
reverse :: [a] -> [a]
reverse lst = foldl f [] lst where f t h = h:t

-- Отображение элементов списка
map :: (a -> b) -> [a] -> [b]
map f = foldr (\x s -> f x : s) []

-- Произведение всех элементов списка
product :: [Integer] -> Integer
product = foldr (*) 1

-- Выделение из списка Maybe всех существующих значений
catMaybes :: [Maybe a] -> [a]
catMaybes l = foldr (\x xs -> case x of 
                                      Just x  -> x:xs 
                                      Nothing -> xs) [] l

-- Диагональ матрицы
diagonal :: [[a]] -> [a]
diagonal ls = if isMatrix ls ls == True 
                          then diagonal' ls 
                          else error "This is not a matrix"
                            where
                              isMatrix [] _     = False
                              isMatrix _ []     = True
                              isMatrix m (l:ls) = if length l == length m then isMatrix m ls else False
                              diagonal' ls = snd $ foldr (\x (f, s) -> (f - 1, x !! f : s)) (length ls - 1, []) ls

-- Фильтр для всех элементов, не соответствующих предикату
filterNot :: (a -> Bool) -> [a] -> [a]
filterNot pr = foldr (\x  l -> if pr x then x : l else  l) []

-- Поиск элемента в списке
elem :: (Eq a) => a -> [a] -> Bool
elem e = foldr (\c cs -> if e == c then True else cs) False

-- Список чисел в диапазоне [from, to) с шагом step
rangeTo :: Integer -> Integer -> Integer -> [Integer]
rangeTo from to step = unfoldr (\l -> if l > to then Nothing else Just(l, l + step)) from 

-- Конкатенация двух списков
append :: [a] -> [a] -> [a]
append a b = foldr (\x s -> x:s ) b a 

-- Разбиение списка lst на куски размером n
-- (последний кусок может быть меньше)
groups :: [a] -> Integer -> [[a]]
groups ls n = unfoldr (\x ->  if null x then Nothing else Just((take $ fi) x, (drop $ fi) x)) ls where fi = fromIntegral n
