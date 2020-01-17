module Task4_1 where

{-
  Задание 4.1
  Реализация монады над функцией.
  Реализуйте все требуемые классы типов и объясните, почему они реализованы именно так.
-}

-- Монада над функцией. В качестве входного значения `fun` может быть что угодно
-- Собственно, почему бы не `String`?
data FunMonad a = FunMonad { fun :: String -> a }

-- реализуйте класс `Functor` для типа `FunMonad`

instance Functor FunMonad where
  fmap f (FunMonad fun) = FunMonad (f . fun)

-- реализуйте класс `Applicative` для типа `FunMonad`

instance Applicative FunMonad where
   pure f = FunMonad(\x -> f)
   (<*>) (FunMonad fl) (FunMonad fr) = FunMonad (\x -> fl x $ fr x)


-- реализуйте класс `Monad` для типа `FunMonad`

instance Monad FunMonad where
  return f = FunMonad (\x -> f)
  (>>=) (FunMonad foo) f = FunMonad (\x -> fun (f (foo x)) x)
  
