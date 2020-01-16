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


treeForTesting = (Node (100,"100")(Node (50,"50")(Leaf)(Leaf))(Node (150,"150")(Node (125,"125")(Leaf)(Leaf))(Node (175,"175")(Leaf)(Leaf))))


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
nearestLE i t = todo

-- Построение дерева из списка пар
treeFromList :: [(Integer, v)] -> TreeMap v
treeFromList []  = Leaf
treeFromList lst = foldr insert Leaf lst

-- Построение списка пар из дерева
listFromTree :: TreeMap v -> [(Integer, v)]
listFromTree t = todo

-- Поиск k-той порядковой статистики дерева
kMean :: Integer -> TreeMap v -> (Integer, v)
kMean i t = todo
