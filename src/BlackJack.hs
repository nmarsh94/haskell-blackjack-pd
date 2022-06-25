module BlackJack where

import Text.Read
import Data.Char
import System.IO
import qualified Data.Text.IO
import Data.List
import System.Random
import qualified Data.Map
import Paths_blackjack



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
   else if map toLower command == "salir" --Si se escribe el comando exit, sale del juego; de lo contrario, puede volver a llamar a un menú
    then 
     putStrLn "\nSaliendo del juego..."
    else do 
     putStrLn "--------------------------------------------------------------------------------------------------------------\nBienvenido a BlackJack, escriba \"Reglas\" o \"Jugar\" para comenzar, \"Salir\" para salir del juego actual: \n\n"
     menu saldoActual saldoObjetivo

