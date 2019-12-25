module Task1_2 where

{-
  Задание 1.2
  Необходимо реализовать четыре любые функции в данном файле
-}

import Todo(todo)

-- синус числа (формула Тейлора)
sin :: Double -> Double
sin x = todo

-- косинус числа (формула Тейлора)
cos :: Double -> Double
cos x = todo

-- наибольший общий делитель двух чисел
gcd' :: Integer -> Integer -> Integer
gcd' 0 0 = error "both zero"
gcd' x 0 = x
gcd' x y = if x < 0 || y < 0 then error "negative" else gcd' y (x `rem` y) 

-- существует ли полный целочисленный квадрат в диапазоне [from, to)?
doesSquareBetweenExist :: Integer -> Integer -> Bool
doesSquareBetweenExist from to = ((floor (sqrt (fromIntegral (to - 1)))) - (ceiling (sqrt (fromIntegral from)))) >= 0

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year   | day < 1 || year < 1582                            = False
                               | elem month [1, 3, 5, 7, 8, 10, 12] && day <= 31   = True
                               | elem month [4, 6, 9, 11]           && day <= 30   = True
                               | month == 2 && day <= 28                           = True
                               | month == 2 && day == 29 = if year `mod` 4 /= 0 then False else forFeb year
                               | otherwise                                         = False
                               where
                                forFeb y | year `mod` 100 /= 0 = True
                                         | year `mod` 400 == 0 = True
                                         | otherwise           = False
                                                                                      

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
pow :: Integer -> Integer -> Integer
pow x y = todo

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime x = if x <= 1 then False else isPrime' x 2
    where
      isPrime' x d | x == d    = True
                   | otherwise = if x `mod` d == 0 then False else isPrime' x (d + 1)

type Point2D = (Double, Double)
-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea points = todo

-- треугольник задан своими координатами.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Point2D -> Point2D -> Point2D -> Integer
triangleKind a b c = 
  let
      segment x y = sqrt $ (fst x - fst y) ^ 2 +  (snd x - snd y) ^ 2 --длинна отрезков треугольника
      -- чтобы проверить треугольник ли это достаточно проверить что его гипотенуза меньше суммы катетов
      isTriangle a b c | (max a $ max b c)  < a + b + c - (max a $ max b c) = whereIsHypo a b c
                       | otherwise                                          = -1
      -- найти гипотенузу
      whereIsHypo a b c | a > c && a > b = pifagor a b c
                        | b > a && b > c = pifagor b c a
                        | c > a && c > b = pifagor c a b
                        | otherwise = error "smt wrong"
      -- погрешность меньше 0.04 градуса riangleKind (11.32, 37.84) (11.42, 16.54) (33.95, 16.63) для прямоугольного (90.04)
      pifagor hypotenus kat1 kat2 | round (hypotenus^2) == round(kat1 ^ 2 + kat2 ^ 2)  = 2  
                                  | hypotenus^2 > kat1 ^ 2 + kat2 ^ 2                  = 0
                                  | hypotenus^2 < kat1 ^ 2 + kat2 ^ 2                  = 1
                                  
  in isTriangle (segment a b) (segment a c) (segment b c) 