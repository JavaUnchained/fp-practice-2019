module Task2_1 where

{-
  Задание 2.1
  На основе структуры бинарного дерева из лекции необходимо реализовать свою структуру данных
  бинарного дерева поиска (без балансировки) и все операции, приведённые в данном файле
-}

import Todo(todo)
import Prelude hiding (lookup)

-- Ассоциативный массив на основе бинарного дерева поиска
-- Ключи - Integer, значения - произвольного типа
data TreeMap v = Node (Integer ,v) (TreeMap v) (TreeMap v)
                | Leaf
    deriving(Show, Eq)

-- Пустое дерево
emptyTree :: TreeMap v
emptyTree = Leaf

-- Содержится ли заданный ключ в дереве?
contains :: TreeMap v -> Integer -> Bool
contains Leaf _ = False
contains (Node (k',v) l r) k | k == k' = True
                             | k >  k' = contains r k
                             | k <  k' = contains l k  

-- Значение для заданного ключа
lookup :: Integer -> TreeMap v -> v
lookup _ Leaf  = error "There is no such key, or this empty tree"
lookup k (Node (k',v) l r) | k == k'   = v
                           | k > k'    = lookup k r
                           | k < k'    = lookup k l 
       

-- Вставка пары (ключ, значение) в дерево
insert :: (Integer, v) -> TreeMap v -> TreeMap v
insert (k, v) Leaf = Node (k, v) Leaf Leaf
insert pair@(k, v) (Node kv@(k',v') l r) | k == k' = Node pair l r
                                         | k >  k' = Node kv l (insert pair r)
                                         | k <  k' = Node kv (insert pair l) r

-- Удаление элемента по ключу
remove :: Integer -> TreeMap v -> TreeMap v
remove _ Leaf= Leaf
remove i (Node p@(k,v) l r)  | i >  k = Node p l (remove i r)
                             | i <  k = Node p (remove i l) r
                             | i == k = checkRight l r
                                where
                                  checkRight l Leaf = l
                                  checkRight l r    = Node (k',v') l r'
                                  ((k', v'), r') = mostMinRight r
                                  mostMinRight (Node (k,v) Leaf r) = ((k, v), r)
                                  mostMinRight (Node (k,v) l    r) = (pair, (Node (k,v) lm r))
                                    where (pair, lm) = mostMinRight l

                                    

-- Поиск ближайшего снизу ключа относительно заданного
nearestLE :: Integer -> TreeMap v -> (Integer, v)
nearestLE _ Leaf = error "There is no such key"
nearestLE i (Node p@(k,v) l r) | i == k = p
                               | i <  k = nearestLE i l
                               | i >  k = underNodes i p r
                                  where
                                      underNodes _ pair Leaf = pair
                                      underNodes i pair node@(Node (k,v) l r) | i < k     = pair
                                                                              | otherwise = nearestLE i node

-- Построение дерева из списка пар
treeFromList :: [(Integer, v)] -> TreeMap v
treeFromList []  = Leaf
treeFromList lst = foldr insert Leaf lst

-- Построение списка пар из дерева
listFromTree :: TreeMap v -> [(Integer, v)]
listFromTree Leaf               = []
listFromTree (Node p@(k,v) l r) = listFromTree l ++ [p] ++ listFromTree r

-- Поиск k-той порядковой статистики дерева
kMean :: Integer -> TreeMap v -> (Integer, v)
kMean _ Leaf = error"kMean is not define, because tree is empty"
kMean i (Node p@(k,v) l r) | size l == i = p
                           | size l >  i = kMean i l
                           | otherwise   = kMean (i - (size l) - 1) r
                              where
                                size Leaf             = 0
                                size (Node (k,v) l r) = 1 + size l + size r
