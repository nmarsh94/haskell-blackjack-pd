module GameFunctions where

import System.Random


data Cartas = A 
              | Dos 
              | Tres 
              | Cuatro 
              | Cinco 
              | Seis 
              | Siete 
              | Ocho 
              | Nueve 
              | Diez 
              | J 
              | Q 
              | K 
              deriving (Show, Enum, Bounded, Read)


repartirCartasCrupier :: StdGen -> Int -> [Cartas] 
repartirCartasCrupier gen total 
 | total < 11 && randomNumber == 1 = toEnum (randomNumber - 1) : repartirCartasCrupier newGen (total + randomNumber + 10) 
 | total < 17 = toEnum (randomNumber - 1) : repartirCartasCrupier newGen (total + randomNumber) 
 | otherwise = [] 
 where (randomNumber,newGen) = randomR(1,13) gen :: (Int,StdGen) 

cartasInicialesJugador :: StdGen -> Int -> [Cartas] 
cartasInicialesJugador gen stop 
 | stop /= 0 = toEnum (randomNumber - 1) : cartasInicialesJugador newGen (stop - 1)
 | otherwise = []
 where (randomNumber,newGen) = randomR(1,13) gen :: (Int,StdGen)


repartirCartaJugador :: StdGen -> [Cartas] 
repartirCartaJugador gen = [toEnum (randomNumber -1)]
 where (randomNumber,newGen) = randomR(1,13) gen :: (Int,StdGen)

primeraCartaCrupier :: StdGen -> [Cartas] 
primeraCartaCrupier gen = [toEnum (randomNumber -1)]
 where (randomNumber,newGen) = randomR(1,13) gen :: (Int,StdGen)



valorDeLaMano :: [Cartas] -> [Int]
valorDeLaMano [] = [] 
valorDeLaMano (c:cx) 
 |fromEnum c >= 10 = fromEnum 10 : valorDeLaMano cx 
 |otherwise = (fromEnum (c) + 1) : valorDeLaMano cx 
    


mostrarCartas :: [Cartas] -> String
mostrarCartas [] = "" 
mostrarCartas (c:cs) 
 |fromEnum c < 10 && fromEnum c > 0 = show (fromEnum (c) + 1) ++ ", " ++ mostrarCartas cs 
 |otherwise = show c ++ ", " ++ mostrarCartas cs 


sumaDeCartas :: [Int] -> Int
sumaDeCartas cartas
 |sum cartas < 12 && 1 `elem` cartas = sum (cartas) + 10 
 |otherwise = sum cartas
