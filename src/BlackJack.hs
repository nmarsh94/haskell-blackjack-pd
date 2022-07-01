module BlackJack where

import Text.Read
import Data.Char
import System.IO
import qualified Data.Text.IO
import Data.List
import System.Random
import qualified Data.Map
import Paths_blackjack
import GameFunctions
import System.Exit (exitSuccess)


main = do
 putStrLn "\n\n<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<          BLACKJACK 21         >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"  
 putStrLn "\n\n**************************************************************************************************************\n"
 putStrLn "Ingrese el saldo objetivo para ganar, ( comienza con la mitad de éste saldo. Objetivo de saldo mínimo de 20 ): \n"
 saldoObjetivo <- getLine
 if (read saldoObjetivo:: Double) > 19 
  then 
   menu ((read saldoObjetivo::Double) / 2::Double) (read saldoObjetivo::Double) 
  else do 
   putStrLn "Ingrese un saldo válido de al menos 20 para su objetivo:" 
   main 
 putStrLn "\nFin del juego, ¿quieres volver a jugar? escriba \"Sí\" o \"No\"\n" 
 command <- getLine
 if map toLower command == "si" 
  then do 
   main
  else if map toLower command == "no" 
   then 
    exitSuccess
   else 
    exitSuccess


menu :: Double -> Double -> IO () 
menu saldoActual saldoObjetivo = do 
 putStrLn "\n\n--------------------------------------------------------------------------------------------------------------\n"
 putStrLn "Bienvenido a BlackJack, escriba \"Reglas\" o \"Jugar\" para comenzar, \"Salir\" para salir del juego actual: \n"  
 command <- getLine
 if map toLower command == "reglas"
  then do 
   filepath <- getDataFileName "reglas.txt"
   handle <- openFile filepath ReadMode 
   contents <- hGetContents handle
   putStrLn contents
   menu saldoActual saldoObjetivo
  else if map toLower command == "jugar"  
   then do 
    putStrLn "\nComenzando el juego...\n"
    generadorCrupier <- newStdGen
    generadorJugador <- newStdGen
    game (primeraCartaCrupier generadorCrupier) (cartasInicialesJugador generadorJugador 2) saldoActual saldoObjetivo 0 
   else if map toLower command == "salir" 
    then 
     putStrLn "\nSaliendo del juego..."
    else do 
     putStrLn "--------------------------------------------------------------------------------------------------------------\n"
     putStrLn "Bienvenido a BlackJack, escriba \"Reglas\" o \"Jugar\" para comenzar, \"Salir\" para salir del juego actual: \n\n"
     menu saldoActual saldoObjetivo


game :: [Cartas] -> [Cartas] -> Double -> Double -> Double -> IO()
game cartasCrupier cartasJugador saldoActual saldoObjetivo apuesta = do
 if saldoActual >= saldoObjetivo 
  then 
   putStrLn $ "\n<<<< Felicidades, hiciste:  " ++ show saldoActual ++ " y tu objetivo era llegar a: " ++ show saldoObjetivo ++ ". Ganaste todo el juego, bien hecho! >> >> "
  else if apuesta == 0
   then do
    putStrLn $ "\n-> Saldo actual que tienes hasta el objetivo: " ++ show saldoActual ++ " / " ++ show saldoObjetivo
    putStrLn "Ingrese la cantidad a apostar en el próximo juego:\n"
    cantidadAApostar <- getLine
    if (read cantidadAApostar::Double) < 1 || (read cantidadAApostar::Double) > saldoActual
     then do 
      putStrLn "<<No es un monto de apuesta válido, ingrese más de 0 y menos de su saldo restante>> \n"
      game cartasCrupier cartasJugador saldoActual saldoObjetivo 0
     else do 
      putStrLn "\n<<Apuesta aceptada!>> \n"
      game cartasCrupier cartasJugador saldoActual saldoObjetivo (read cantidadAApostar::Double)
   else if (sumaDeCartas $ valorDeLaMano cartasJugador) == 21 && (length cartasJugador) == 2
     then do
      putStrLn $ "\n>> Cartas del crupier: " ++ mostrarCartas cartasCrupier ++ "      -> Total: " ++ show (sumaDeCartas $ valorDeLaMano cartasCrupier) ++ "\n"
      putStrLn $ ">> Tus cartas: " ++ mostrarCartas cartasJugador ++ "      -> Total: " ++ show (sumaDeCartas $ valorDeLaMano cartasJugador) ++ "\n"
      putStrLn "Tienes BlackJack, ganas 1.5 veces el monto de tu apuesta!"
      generadorCrupier <- newStdGen
      generadorJugador <- newStdGen
      game (primeraCartaCrupier generadorCrupier) (cartasInicialesJugador generadorJugador 2) (saldoActual + (apuesta / 2) + apuesta) saldoObjetivo 0  
     else if (sumaDeCartas $ valorDeLaMano cartasCrupier) == 21 && (length cartasCrupier) == 2
       then do
        putStrLn $ "\n>> Cartas del crupier: " ++ mostrarCartas cartasCrupier ++ "      -> Total: " ++ show (sumaDeCartas $ valorDeLaMano cartasCrupier) ++ " \n"
        putStrLn $ ">> Tus cartas: " ++ mostrarCartas cartasJugador ++ "      -> Total: " ++ show (sumaDeCartas $ valorDeLaMano cartasJugador) ++ "\n"
        putStrLn "El crupier tiene BlackJack, has perdido!"
        if (saldoActual - apuesta) < 1
         then
          putStrLn "\n<<<<Perdiste todo tu dinero, se acabó el juego>>>>"
        else do 
         generadorCrupier <- newStdGen
         generadorJugador <- newStdGen
         game (primeraCartaCrupier generadorCrupier) (cartasInicialesJugador generadorJugador 2) (saldoActual - apuesta) saldoObjetivo 0
       else if (sumaDeCartas $ valorDeLaMano cartasCrupier) > 21 
        then do 
         putStrLn $ "\n>> Cartas del crupier: " ++ mostrarCartas cartasCrupier ++ "      -> Total: " ++ show (sumaDeCartas $ valorDeLaMano cartasCrupier) ++ "\n"
         putStrLn $ ">> Tus cartas: " ++ mostrarCartas cartasJugador ++ "      -> Total: "++ show (sumaDeCartas $ valorDeLaMano cartasJugador) ++  "\n"
         putStrLn "El crupier se pása de 21 puntos, tu ganas!"
         generadorCrupier <- newStdGen
         generadorJugador <- newStdGen
         game (primeraCartaCrupier generadorCrupier) (cartasInicialesJugador generadorJugador 2) (saldoActual + apuesta) saldoObjetivo 0  
        else if (sumaDeCartas $ valorDeLaMano cartasJugador) > 21 
         then do
          putStrLn $ "\n>> Cartas del crupier: " ++ mostrarCartas cartasCrupier ++ "      -> Total: " ++ show (sumaDeCartas $ valorDeLaMano cartasCrupier) ++ " \n"
          putStrLn $ ">> Tus cartas: " ++ mostrarCartas cartasJugador ++ "      -> Total: " ++ show (sumaDeCartas $ valorDeLaMano cartasJugador) ++ "\n"
          putStrLn "Te pasas de 21 puntos, has perdido"
          if (saldoActual - apuesta) < 1
           then
            putStrLn "\n<<<<Perdiste todo tu dinero, se acabó el juego>>>>"
           else do 
            generadorCrupier <- newStdGen
            generadorJugador <- newStdGen
            game (primeraCartaCrupier generadorCrupier) (cartasInicialesJugador generadorJugador 2) (saldoActual - apuesta) saldoObjetivo 0
         else if (((sumaDeCartas $ valorDeLaMano cartasJugador) == (sumaDeCartas $ valorDeLaMano cartasCrupier)) && (length cartasCrupier) > 1)
          then do
           putStrLn $ "\n>> Cartas del crupier: " ++ mostrarCartas cartasCrupier ++ "      -> Total: " ++ show (sumaDeCartas $ valorDeLaMano cartasCrupier) ++ "\n"
           putStrLn $ ">> Tus cartas: " ++ mostrarCartas cartasJugador ++ "      -> Total: " ++ show (sumaDeCartas $ valorDeLaMano cartasJugador) ++ "\n"
           putStrLn "Empataste, no ganas ni pierdes dinero esta vez!"
           generadorCrupier <- newStdGen
           generadorJugador <- newStdGen
           game (primeraCartaCrupier generadorCrupier) (cartasInicialesJugador generadorJugador 2) saldoActual saldoObjetivo 0
          else if (((sumaDeCartas $ valorDeLaMano cartasJugador) > (sumaDeCartas $ valorDeLaMano cartasCrupier)) && (length cartasCrupier) > 1)
           then do
            putStrLn $ "\n>> Cartas del crupier: " ++ mostrarCartas cartasCrupier ++ "      -> Total: " ++ show (sumaDeCartas $ valorDeLaMano cartasCrupier) ++ "\n"
            putStrLn $ ">> Tus cartas: " ++ mostrarCartas cartasJugador ++ "      -> Total: " ++ show (sumaDeCartas $ valorDeLaMano cartasJugador) ++ "\n"
            putStrLn "Tienes mas puntos, tu ganas!"
            generadorCrupier <- newStdGen
            generadorJugador <- newStdGen
            game (primeraCartaCrupier generadorCrupier) (cartasInicialesJugador generadorJugador 2) (saldoActual + apuesta) saldoObjetivo 0  
           else if (((sumaDeCartas $ valorDeLaMano cartasJugador) < (sumaDeCartas $ valorDeLaMano cartasCrupier)) && (length cartasCrupier) > 1)
            then do
             putStrLn $ "\n>> Cartas del crupier: " ++ mostrarCartas cartasCrupier ++ "      -> Total: " ++ show (sumaDeCartas $ valorDeLaMano cartasCrupier) ++ "\n"
             putStrLn $ ">> Tus cartas: " ++ mostrarCartas cartasJugador ++ "      -> Total: " ++ show (sumaDeCartas $ valorDeLaMano cartasJugador) ++ "\n"
             putStrLn "Tienes menos puntos, has perdido"
             if (saldoActual - apuesta) < 1
              then
               putStrLn "\n<<<<Perdiste todo tu dinero, se acabó el juego>>>>"
              else do 
               generadorCrupier <- newStdGen
               generadorJugador <- newStdGen
               game (primeraCartaCrupier generadorCrupier) (cartasInicialesJugador generadorJugador 2) (saldoActual - apuesta) saldoObjetivo 0
            else do 
             putStrLn $ "\n>> Cartas del crupier: " ++ mostrarCartas cartasCrupier ++ "      -> Total: " ++ show (sumaDeCartas $ valorDeLaMano cartasCrupier) ++ "\n"
             putStrLn $ ">> Tus cartas: " ++ mostrarCartas cartasJugador ++ "      -> Total: " ++ show (sumaDeCartas $ valorDeLaMano cartasJugador) ++ "\n"
             putStrLn "Quieres pedir una carta o plantarte? (Escribe \"p\" para pedir una carta, o \"pl\" para plantarte): \n"
             command <- getLine
             if map toLower command == "p" 
              then do
               generadorJugador <- newStdGen
               let nuevaCarta = repartirCartaJugador generadorJugador
               let cartasActualesJugador = cartasJugador ++ nuevaCarta
               game cartasCrupier cartasActualesJugador saldoActual saldoObjetivo apuesta
              else if map toLower command == "pl"
               then do
                generadorCrupier <- newStdGen
                let primeraCarta = sumaDeCartas $ valorDeLaMano $ primeraCartaCrupier generadorCrupier
                let cartasC = repartirCartasCrupier generadorCrupier primeraCarta
                let cartasActualesCrupier = cartasCrupier ++ cartasC
                game cartasActualesCrupier cartasJugador saldoActual saldoObjetivo apuesta 
               else do 
                putStrLn "--------------------------------------------------------------------------------------------------------------\n"
                putStrLn "Bienvenido a BlackJack, escriba \"Reglas\" o \"Jugar\" para comenzar, \"Salir\" para salir del juego actual: \n\n"
                menu saldoActual saldoObjetivo
              