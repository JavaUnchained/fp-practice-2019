module Task5_2 where

import Todo(todo)
import Prelude hiding (concat)
-- Зиппер из лекции 

data Zipper a = Zipper [a] [a]

-- Реализуйте экземпляры классов Show и Eq для этого типа
instance (Show a) => Show (Zipper a) where
    show (Zipper l r) = (show $ reverse l) ++ (show r)

instance (Eq a) => Eq (Zipper a) where
    (==) a a' = innerCon a == innerCon a' 

fromList :: [a] -> Zipper a
fromList lst = Zipper [] lst

goRight :: Zipper a -> Zipper a
goRight z@(Zipper _ []) = z
goRight (Zipper l (rh:rt)) = Zipper (rh:l) rt

goLeft :: Zipper a -> Zipper a
goLeft z@(Zipper [] _) = z
goLeft (Zipper (lh:lt) r) = Zipper lt (lh:r)

putRight :: a -> Zipper a -> Zipper a
putRight x (Zipper l r) = Zipper l (x:r)

putLeft :: a -> Zipper a -> Zipper a
putLeft x (Zipper l r) = Zipper (x:l) r

removeRight :: Zipper a -> Zipper a
removeRight (Zipper l (_:rt)) = Zipper l rt

removeLeft :: Zipper a -> Zipper a
removeLeft (Zipper (_:lt) r) = Zipper lt r

-- Используя приведённые выше функции, реализуйте функцию конкатенации
-- вставки подсписка в середину и выделения подсписка

concat :: Zipper a -> Zipper a -> Zipper a
concat (Zipper l r) (Zipper l' r') = Zipper (l ++ r)  (l' ++ r')

-- просто конкатенация зиппера в список
innerCon :: Zipper a -> [a]
innerCon (Zipper l r) = l ++ r

insertManyAt :: Int -> Zipper a -> Zipper a -> Zipper a
insertManyAt _ (Zipper [] []) into = into
insertManyAt _ what (Zipper [] []) = what
insertManyAt index w@(Zipper l r) i@(Zipper l' r') | index > (length $ innerCon i) || index < 0 = error"Incorrect index"
                                                   | otherwise  = helper index w $ fromList $ innerCon i
                                                    where
                                                        helper ind w' in' | ind == 0  = Zipper (l' ++ l) (r ++ r')
                                                                          | otherwise = helper (ind-1) w' (goRight in')

subZipper :: Int -> Int -> Zipper a -> Zipper a
subZipper _    _   (Zipper [] []) = error"Zipper is empty"
subZipper from to i@(Zipper l r) | from < 0 || to < 0 || from > to || from > (ln i) || to > (ln i) = error"Incorrect from/to define"
                                 | otherwise = Zipper [] (take (to - from + 1) $ drop from (l ++ r ))
                                    where ln x = length $ innerCon x