module BlackJack where

import Text.Read
import Data.Char
import System.IO
import qualified Data.Text.IO
import Data.List
import System.Random
import qualified Data.Map
import Paths_blackjack
import qualified GameFunctions as GF



main = do
 putStrLn "\n\n**************************************************************************************************************\nIngrese el saldo objetivo para ganar, ( comienza con la mitad de este saldo, objetivo de saldo mínimo de 20 ): \n" 
 saldoObjetivo <- getLine
 if (read saldoObjetivo:: Double) > 19 
  then 
   menu ((read saldoObjetivo::Double) / 2::Double) (read saldoObjetivo::Double) 
  else do 
   putStrLn "Ingrese un saldo válido de al menos 20 para su objetivo:" 
   main 
 putStrLn "\nFin del juego, ¿quieres volver a jugar? escriba \"Sí\" o \"No\"\n" 
 command <- getLine
 if map toLower command == "si" -- si doy sí, simplemente volvemos a al main de nuevo. 
  then do 
   main
  else if map toLower command == "no" 
   then 
    putStrLn "\nGracias por jugar BlackJack, adiós "
   else 
    putStrLn "\nNo entendí tu elección, adiós."


menu :: Double -> Double -> IO () 
menu saldoActual saldoObjetivo = do --tenemos el saldo actual y el saldo objetivo pasado aquí
 putStrLn "\n\n--------------------------------------------------------------------------------------------------------------\nBienvenido a BlackJack, escriba \"Reglas\" o \"Jugar\" para comenzar, \"Salir\" para salir del juego actual: \n"  
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
    game (GF.repartirCartasCrupier generadorCrupier 0) (GF.cartasInicialesJugador generadorJugador 2) saldoActual saldoObjetivo 0 
   else if map toLower command == "salir" --Si se escribe el comando exit, sale del juego; de lo contrario, puede volver a llamar a un menú
    then 
     putStrLn "\nSaliendo del juego..."
    else do 
     putStrLn "--------------------------------------------------------------------------------------------------------------\nBienvenido a BlackJack, escriba \"Reglas\" o \"Jugar\" para comenzar, \"Salir\" para salir del juego actual: \n\n"
     menu saldoActual saldoObjetivo


game :: [GF.Cartas] -> [GF.Cartas] -> Double -> Double -> Double -> IO()
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
   else if (GF.sumaDeCartas $ GF.valorDeLaMano cartasCrupier) > 21 
    then do 
     putStrLn $ "\n>> Cartas del crupier: " ++ GF.mostrarCartas cartasCrupier ++ "\n"
     putStrLn $ ">> Tus cartas: " ++ GF.mostrarCartas cartasJugador ++ "\n"
     putStrLn "El crupier se pása de 21 puntos, tu ganas!"
     generadorCrupier <- newStdGen
     generadorJugador <- newStdGen
     game (GF.repartirCartasCrupier generadorCrupier 0) (GF.cartasInicialesJugador generadorJugador 2) (saldoActual + apuesta) saldoObjetivo 0
    else if (GF.sumaDeCartas $ GF.valorDeLaMano cartasJugador) > 21 
     then do
      putStrLn $ "\n>> Cartas del crupier: " ++ GF.mostrarCartas cartasCrupier ++ "\n"
      putStrLn $ ">> Tus cartas: " ++ GF.mostrarCartas cartasJugador ++ "\n"
      putStrLn "Te pasas de 21 puntos, has perdido"
      if (saldoActual - apuesta) < 1
       then
        putStrLn "\n<<<<Perdiste todo tu dinero, se acabó el juego>>>>"
       else do 
        generadorCrupier <- newStdGen
        generadorJugador <- newStdGen
        game (GF.repartirCartasCrupier generadorCrupier 0) (GF.cartasInicialesJugador generadorJugador 2) (saldoActual - apuesta) saldoObjetivo 0
     else if (GF.sumaDeCartas $ GF.valorDeLaMano cartasJugador) == 21 
      then do
       putStrLn $ "\n>> Cartas del crupier: " ++ GF.mostrarCartas cartasCrupier ++ "\n"
       putStrLn $ ">> Tus cartas: " ++ GF.mostrarCartas cartasJugador ++ "\n"
       putStrLn "Tienes BlackJack, ganas 1.5 veces el monto de tu apuesta!"
       generadorCrupier <- newStdGen
       generadorJugador <- newStdGen
       game (GF.repartirCartasCrupier generadorCrupier 0) (GF.cartasInicialesJugador generadorJugador 2) (saldoActual + (apuesta / 2) + apuesta) saldoObjetivo 0
      else if (GF.sumaDeCartas $ GF.valorDeLaMano cartasJugador) == (GF.sumaDeCartas $ GF.valorDeLaMano cartasCrupier) 
       then do
        putStrLn $ "\n>> Cartas del crupier: " ++ GF.mostrarCartas cartasCrupier ++ "\n"
        putStrLn $ ">> Tus cartas: " ++ GF.mostrarCartas cartasJugador ++ "\n"
        putStrLn "Empataste, no ganas ni pierdes dinero esta vez!"
        generadorCrupier <- newStdGen
        generadorJugador <- newStdGen
        game (GF.repartirCartasCrupier generadorCrupier 0) (GF.cartasInicialesJugador generadorJugador 2) saldoActual saldoObjetivo 0
       else do 
        putStrLn $ "\n>> Cartas del crupier: " ++ GF.mostrarCartas cartasCrupier ++ "\n"
        putStrLn $ ">> Tus cartas: " ++ GF.mostrarCartas cartasJugador ++ "\n"
        putStrLn "Quieres pedir una carta o plantarte? (Escribe \"pedir\" o \"plantarme\"): \n"
        command <- getLine
        if map toLower command == "pedir" 
         then do
          generadorJugador <- newStdGen
          let nuevaCarta = GF.repartirCartaJugador generadorJugador
          let cartasActualesJugador = cartasJugador ++ nuevaCarta
          game cartasCrupier cartasActualesJugador saldoActual saldoObjetivo apuesta
         else if map toLower command == "plantarme"
          then if (GF.calcularGanador cartasCrupier cartasJugador) == "ganaste" 
           then do 
            putStrLn $ "\n>> Cartas del crupier: " ++ GF.mostrarCartas cartasCrupier ++ "\n"
            putStrLn $ ">> Tus cartas: " ++ GF.mostrarCartas cartasJugador ++ "\n"
            putStrLn "Tienes mas puntos, has ganado!"
            generadorCrupier <- newStdGen
            generadorJugador <- newStdGen
            game (GF.repartirCartasCrupier generadorCrupier 0) (GF.cartasInicialesJugador generadorJugador 2) (saldoActual + apuesta) saldoObjetivo 0
           else if (GF.calcularGanador cartasCrupier cartasJugador) == "perdiste" 
            then do
             putStrLn $ "\n>> Cartas del crupier: " ++ GF.mostrarCartas cartasCrupier ++ "\n"
             putStrLn $ ">> Tus cartas: " ++ GF.mostrarCartas cartasJugador ++ "\n"
             putStrLn "El crupier tiene mas puntos, has perdido!"
             if (saldoActual - apuesta) < 0
              then 
               putStrLn "Perdiste todo tu dinero, se acabó el juego"
              else do 
               generadorCrupier <- newStdGen
               generadorJugador <- newStdGen
               game (GF.repartirCartasCrupier generadorCrupier 0) (GF.cartasInicialesJugador generadorJugador 2) (saldoActual - apuesta) saldoObjetivo 0
            else do
             putStrLn $ "\n>> Cartas del crupier: " ++ GF.mostrarCartas cartasCrupier ++ "\n"
             putStrLn $ ">> Tus cartas: " ++ GF.mostrarCartas cartasJugador ++ "\n"
             putStrLn "Empataste, no ganas ni pierdes dinero esta vez!!"
             generadorCrupier <- newStdGen
             generadorJugador <- newStdGen
             game (GF.repartirCartasCrupier generadorCrupier 0) (GF.cartasInicialesJugador generadorJugador 2) saldoActual saldoObjetivo 0
          else do
           putStrLn "Quieres pedir una carta o plantarte? (Escribe \"pedir\" o \"plantarme\"): \n"
           game cartasCrupier cartasJugador saldoActual saldoObjetivo apuesta
