{-# LANGUAGE AllowAmbiguousTypes #-}
import Text.XHtml (base)


-- 1. Retornar la longitud de una lista sin usar la función primitiva length.

loa :: [a] -> Int
loa [] = 0
loa (_ : xs) = 1 + loa xs

-- 2. Retornar la longitud de una lista sin usar la función primitiva length, pero utilizando un acumulador.
longitudlista1 :: [a] -> Int
longitudlista1 xs = longAux xs 0
  where
    longAux [] acum = acum
    longAux (_ : xs) acum = longAux xs (acum + 1)

-- 2. Dada una lista y un elemento, contar cuantas veces se encuentra dicho elemento en la lista.
cuantasveces :: (Eq s) => [s] -> s -> Int
cuantasveces [] _ = 0
cuantasveces xs a = cuantv xs a 0
  where
    cuantv [] a acumulador = acumulador
    cuantv (x : xs) a acumulador =
      if (a == x)
        then cuantv xs a (acumulador + 1)
        else cuantv xs a acumulador

-- 3. Dada una lista y un elemento, retornar un valor booleano que indique si se encuentra el elemento en la lista.
seencuentra :: (Eq a) => [a] -> a -> Bool
seencuentra xs a = elem a xs

-- 4  Dada una lista de entrada, generar otra de doble longitud, donde cada elemento figure dos veces. Ud. elige el orden que tendrá su solución.
duplicate :: [a] -> [a]
duplicate [] = []
duplicate (x : xs) = x : x : duplicate xs

duplicate1 xs = concatMap (\x -> [x, x]) xs

-- 5. Dadas dos listas, retorne un 1 si la primera tiene mayor cantidad de elementos que la segunda, un
-- 2 en caso contrario y un 0 si tienen igual cantidad de elementos.

cantelem :: [a] -> [a] -> Int
cantelem a b
  | length a > length b = 1
  | length a < length b = 2
  | otherwise = 0

cantanelem :: [a] -> [a] -> Int
cantanelem a b
  | aux a < aux b = 2
  | aux a > aux b = 1
  | otherwise = 0
  where
    aux [] = 0
    aux (_ : xs) = 1 + aux xs

-- 6. Tomando como entrada una lista de números, retornar el producto de sus componentes.
coso :: [Int] -> Int
coso [] = 0
coso a = foldr (*) 1 a

coso1 :: [Int] -> Int
coso1 [] = 0
coso1 xs = aux xs 1
  where
    aux [] acumulador = acumulador
    aux (x : xs) acumulador = (x * acumulador) * aux xs acumulador

-- 7. Tomando como entrada una lista de dos componentes [ a, b ], retornar el producto de a * b por
-- sumas sucesivas
evaluacion :: [Int] -> Int
evaluacion [a, b]
  | b < 0 = negate (multiplica a (negate b))
  | otherwise = multiplica a b
  where
    multiplica _ 0 = 0
    multiplica x y = x + multiplica x (y - 1)
evaluacion _ = error "La lista debe contener exactamente 2 elementos"

-- 8. Dada una lista, retornar el reverso de la misma, sin usar la función predefinida reverse
revertir :: [a] -> [a]
revertir [] = []
revertir (x : xs) = revertir xs ++ [x]

revertir1 :: [a] -> [a]
revertir1 xs = aux xs []
  where
    aux [] acc = acc
    aux (x : xs) acc = aux xs (x : acc)

-- 8. Dada una lista, retornar el reverso de la misma, sin usar la función predefinida reverse
-- Entrada--> [[1,2,3],[4,5],[2]] Salida --> [[3,2,1],[5,4],[2]]
revertit :: [[a]] -> [[a]]
revertit [] = []
revertit (x : xs) = revertit xs ++ [x]

reverseListR :: [a] -> [a]
reverseListR xs = foldr (\x acc -> acc ++ [x]) [] xs

-- Ejemplo: reverseListR [1,2,3,4] → ( [ ] ++ [4] ) ++ ...
-- El resultado final es [4,3,2,1]

------------------------------------------------------------------------------------------
-- Repaso 1er recuperacion
cantlongN :: (Num b, Foldable t) => [t a] -> Int -> b
-- a) Definiendo una función recursiva. (0.8Pts)
cantlongN [] _ = 0
cantlongN (x : xs) n = if ((length x) == n) then 1 + cantlongN xs n else cantlongN xs n

longNCont :: (Num b, Foldable t) => [t a] -> Int -> b
longNCont lis n = aux lis n 0
  where
    aux [] n acum = acum
    aux (x : xs) n acum = if (length x == n) then aux xs n (acum + 1) else aux xs n acum
--La cual dada una lista de listas y un entero N devolver la cantidad de listas interiores de
--longitud igual a N.
longNfoldr :: (Num b, Foldable t) => [t a] -> Int -> b
longNfoldr lis n = foldr (\lista acc -> if length lista == n then 1 + acc else acc) 0 lis

longNfoldl :: (Num b, Foldable t) => [t a] -> Int -> b
longNfoldl lis n = foldl (\acc lista -> if length lista == n then acc + 1 else acc) 0 lis
----------------------------------------------
--La cual toma como entrada una lista de tuplas de 2 elementos numéricos y retorna una
--tupla con dos números, donde el primero corresponde a la suma de los 1eros elementos
--de las tuplas y el 2do al producto de los que están en la posición 2 de las tuplas de la
--lista de entrada a la función.
unzSumProd :: (Num a,Num b) => [(a, b)] -> (a, b)
unzSumProd [] = (0,0)
unzSumProd xs = aux xs (0,1)
  where
    aux [] (s,c) = (s,c)
    aux ((a,b):xs) (s,c) = aux xs (s+a,b*c)
    
-- ahora haz lo mismo sun acumuladores:
unzsumprodsin :: (Num a, Num b) => [(a,b)] -> (a,b)
unzsumprodsin [] = (0,0)
unzsumprodsin [(a,b)]= (a,b)
unzsumprodsin ((a,b):xs) = (a+s , b* p)
  where 
    (s,p) = unzsumprodsin xs
--ahora con foldl
unzfoldl :: (Num a, Num b) => [(a,b)] -> (a,b)
unzfoldl xs = foldl (\(a,b) (s,p) -> (a+s,b*p)) (0,1) xs

unzfoldr :: (Num a, Num b) => [(a,b)] -> (a,b)
unzfoldr xs = foldr (\(a,b)(c,d)->(a+c,b*d)) (0,1) xs 


fun :: (Eq a, Num b) => a -> [a] -> b
fun e (x:xs) = foldl(\ a y -> if (e==y) then a+1 else a) 0 (x:xs) 
