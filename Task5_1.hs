module Task5_1 where

import Todo(todo)

-- Структура двусвязного списка из лекции про ленивость

data DList a = DNil 
             | DCons { 
                left :: (DList a), 
                current :: a, 
                right :: (DList a) 
             }

instance (Show a) => Show (DList a) where
    show it = "[" ++ showBody it ++ "]"
              where showBody DNil = ""
                    showBody (DCons _ h DNil) = show h
                    showBody (DCons _ h t) = show h ++ ", " ++ showBody t

instance (Eq a) => Eq (DList a) where
    DNil == DNil = True
    (DCons _ h1 t1) == (DCons _ h2 t2) = h1 == h2 && t1 == t2
    _ == _ = False

list2dlist :: [a] -> DList a
list2dlist lst = list2dlist' DNil lst

list2dlist' :: DList a -> [a] -> DList a
list2dlist' _ [] = DNil
list2dlist' left (h: t) = 
    let rec = DCons left h (list2dlist' rec t)
    in rec


-- Реализуйте функции индексирования, вставки и удаления элементов

index :: DList a -> Int -> a
index DNil _ = error"Can't indexing. List is empty."
index (DCons l c r) i | i <  0    = error"Index less then zero"
                      | i == 0    = c  
                      | otherwise = index r (i -1)


insertAt :: DList a -> Int -> a -> DList a
insertAt DNil index value | index == 0  = DCons DNil value DNil
                          | otherwise   = error"Incorrect index for empty list"
insertAt (DCons l c r) index value | index < 0  = error"Index less then zero"
                                   | index > 0  = DCons l c (insertAt r (index - 1) value)
                                   | index == 0 = m 
                                        where
                                            m = DCons l value n
                                            n = DCons m c r


removeAt :: DList a -> Int -> DList a
removeAt DNil _ = error"Can't remove. List is empty"
removeAt (DCons _ _ DNil) 0 = DNil
removeAt (DCons _ _ DNil) _ = error"Can't remove. This index out of bounds "
removeAt (DCons l c r@(DCons _ c' r')) index | index == 0 = DCons l c' r'
                                             | otherwise  = DCons l c $ removeAt r $ index -1
