--EJ1
cantlongN::(Num b, Foldable t)=>[t a] -> Int -> b 
cantlongN[] _ = 0
cantlongN (x:xs) n = if length x > n then 1 + cantlongN xs n else cantlongN xs n

--EJ1-b
cantlongN1::(Num b, Foldable t)=>[t a] -> Int -> b 
--caso de salida vacio
cantlongN1 [] _ = 0
--caso de salida recursividad
cantlongN1 (x:xs) n = longAux (x:xs) n 0
--definicio recursividad
longAux::(Num b, Foldable t)=>[t a] -> Int -> b -> b    
--caso vacio de recursividad
longAux [] _ acum = acum  
--funcion recursiva
longAux (x:xs) n acum = if length x == n then longAux xs n (acum + 1) else longAux xs n acum

--EJ1-c
cantlongN2::(Num b, Foldable t)=>[t a] -> Int -> b 
cantlongN2 xs n = foldl (\acc x -> if length x > n then acc + 1 else acc) 0 xs

--EJ2
unzSumProd:: (Num a,Num b) => [(a,b)] -> (a,b)
unzSumProd [] = (0,1)
unzSumProd ((x,y):xs) = (x + x1, y * y1)
  where (x1,y1) = unzSumProd xs

--EJ2-acumulador

unzSumProd1:: (Num a,Num b) => [(a,b)] -> (a,b)
unzSumProd1 [] = (0,1)
unzSumProd1 ((x,y):xs) = faux ((x,y):xs) (0,1)
faux [] acc = acc
faux ((a,b):xs) (x,y) =  faux xs (a+x, y*b)

--con foldl y de dos listas devolvemos una lista que tiene todos los elementos que no estan en la otra, por ej L1-L2

diferencia:: Eq a => [a] -> [a] -> [a]
diferencia [] _ = []
diferencia list1 list2= snd( foldl(\z x -> if (elem x (fst z)) then z else (fst z,snd z ++ [x])) (list2,[]) list1 )
--L1 y L2 son las dos listas que se van a comparar y la funcion devuelve una lista con los elementos de L1 que no estan en L2.

--recursion impura con foldl osea con acumulador
--recursividad pura con foldr baja y resuelve a la vuelta 

fun n (x:xs) = foldr (aux n) (0,0) (x:xs)
  where
    aux n x z = if (n==x) then (fst z + 1, snd z + n) else (fst z, snd z)

    -- o con lambda reemplazo el (aux n) por \x z -> if (n==x) then (fst z + 1, snd z + n) else (fst z, snd z)
    -- en fold cuando una lista se vacia devuelve z o lambda

--que hace esta funcion? cuenta cuantas veces aparece n en la lista y devuelve la suma de todos los n que aparecen en la lista.