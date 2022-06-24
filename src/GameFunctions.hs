module GameFunctions where

import System.Random


data Cartas = A 
              | Two 
              | Three 
              | Four 
              | Five 
              | Six 
              | Seven 
              | Eight 
              | Nine 
              | Ten 
              | J 
              | Q 
              | K 
              deriving (Show, Enum, Bounded, Read)


repartirCartasCrupier :: StdGen -> Int -> [Cartas] 
repartirCartasCrupier gen total 
 | total < 11 && randomNumber == 1 = toEnum (randomNumber - 1) : repartirCartasCrupier newGen (total + randomNumber + 10) 
 | total < 17 = toEnum (randomNumber - 1) : repartirCartasCrupier newGen (total + randomNumber) 
 | otherwise = [] --De lo contrario, pone una lista vacía que le da la lista de cartas. Se detiene en la lista vacía. 
 where (randomNumber,newGen) = randomR(1,13) gen :: (Int,StdGen) 

cartasInicialesJugador :: StdGen -> Int -> [Cartas] 
cartasInicialesJugador gen stop 
 | stop /= 0 = toEnum (randomNumber - 1) : cartasInicialesJugador newGen (stop - 1)
 | otherwise = []
 where (randomNumber,newGen) = randomR(1,13) gen :: (Int,StdGen)

--cuando elijan hit esto se llamará 
repartirCartaJugador :: StdGen -> [Cartas] 
repartirCartaJugador gen = [toEnum (randomNumber -1)]
 where (randomNumber,newGen) = randomR(1,13) gen :: (Int,StdGen)


--mostrar el valor de las cartas. Te da las cartas y te devuelve una lista de ints 
valorDeLaMano :: [Cartas] -> [Int]
valorDeLaMano [] = [] 
valorDeLaMano (c:cx) 
 |fromEnum c >= 10 = fromEnum 10 : valorDeLaMano cx 
 |otherwise = (fromEnum (c) + 1) : valorDeLaMano cx 
    

--Show cards mostrará las cartas como una cadena. 
mostrarCartas :: [Cartas] -> String
mostrarCartas [] = "" 
mostrarCartas (c:cs) 
 |fromEnum c < 10 && fromEnum c > 0 = show (fromEnum (c) + 1) ++ ", " ++ mostrarCartas cs 
 |otherwise = show c ++ ", " ++ mostrarCartas cs 


sumaDeCartas :: [Int] -> Int
sumaDeCartas cards
 |sum cards < 12 && 1 `elem` cards = sum (cards) + 10 
 |otherwise = sum cards


--calcularGanador va a devolver una cadena string. Esto se usa en STAND y en todas las funciones del juego principal main para ver quién gana. 
calcularGanador :: [Cartas] -> [Cartas] -> String
calcularGanador cartasCrupier cartasJugador 
 |sumaDeCartas (valorDeLaMano cartasCrupier) == sumaDeCartas (valorDeLaMano cartasJugador) = "empate"
 |sumaDeCartas (valorDeLaMano cartasCrupier) > sumaDeCartas (valorDeLaMano cartasJugador) = "perdiste"
 |sumaDeCartas (valorDeLaMano cartasCrupier) < sumaDeCartas (valorDeLaMano cartasJugador) = "ganaste"