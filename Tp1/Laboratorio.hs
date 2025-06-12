module PilaTDA (Pila, vaciaP, esVacia, push, pop, copy) where

import Prelude

data Pila a = Pila [a]

instance (Show a) => Show (Pila a) where
    show (Pila xs) = show xs

vaciaP = Pila []

esVacia (Pila []) = True
esVacia (Pila (_ : _)) = False

-- push = insertar
push :: a -> Pila a -> Pila a
push x (Pila xs) = Pila (x : xs)


-- pop = suprimir
pop :: Pila a -> Pila a
pop (Pila []) = vaciaP
pop (Pila (x : xs)) = (Pila xs)

copy (Pila (x : xs)) = x

--enplila es para sabre si un elemento está en la pila
enPila:: Eq a => a -> Pila a -> Bool
enPila _ (Pila []) = False
enPila x (Pila (y:ys)) = if x == y then True else enPila x (Pila ys)

-- cuantos devuelve la cantidad de elementos en la pila
cuantos:: Pila a -> Int
cuantos (Pila []) = 0
cuantos (Pila (_ : xs)) = 1 + cuantos (Pila xs)

-- aplicar aplica una función a cada elemento de la pila
aplicar:: (a1 -> a2) -> Pila a1 -> Pila a2
aplicar f (Pila xs) = Pila (map f xs)

iguales:: Eq a => Pila a -> Pila a -> Bool
iguales (Pila []) (Pila []) = True
iguales (Pila (x:xs)) (Pila (y:ys)) = if x == y then iguales (Pila xs) (Pila ys) else False
iguales _ _ = False



