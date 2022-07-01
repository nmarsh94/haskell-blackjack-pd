# Trabajo final - Programación Declarativa

## BlackJack en Haskell

&nbsp;

## Tema del proyecto
Se desarrolló un programa que permite al usuario jugar al juego de cartas **BlackJack**.
El juego ha sido implementado en Haskell.

&nbsp;

##   Definición
Antes de que comience cada partida, primero se le pedirá que ingrese un valor de juego objetivo, ganará el juego en general si llega a su valor objetivo y perderá si llega a cero.
Recibirás la mitad de esta cantidad antes de empezar a jugar, y podrás elegir qué cantidad apostar cada vez que hayas ganado, perdido o empatado una mano contra el crupier.
Se paga 1.5 veces la apuesta si tienes 21 puntos

Este juego consiste en enfrentarse de forma individual a la banca comparando su mano contra la de la banca o crupier, intentando conseguir 21 puntos o el número más cercano posible sin pasarse. Para conseguir dicha puntuación se suman los valores de dos cartas que se reparten al inicio, con los de las nuevas cartas que, opcionalmente, se podrán añadir si el jugador pide otra carta. Si las dos cartas iniciales suman 21, se denomina Blackjack, y es la mejor jugada. Cuando un jugador no suma 21 con sus dos cartas podrá pedir cartas para conseguir dicho número o uno cercano, pero si el jugador se pasa de esos 21 puntos pierde, indistintamente de lo que haga la banca. Si se produce un empate, la apuesta no se pierde ni se gana; la apuesta solo se modifica si gana o pierde la mano. El jugador elige pedir o plantarse en cada carta extraída. Un crupier debe continuar sacando cartas después de su primera carta hasta que tenga 17 puntos o más, y se detiene. Esto significa que el crupier repartirá cartas automáticamente hasta que tenga 17 puntos o más. Con 22 puntos o más, el crupier pierde. 
Todas las cartas del 2 al 10 valen sus respectivos puntos. La A vale 1 u 11 puntos. J Q K valen 10 puntos.
El As cambiará de valor dependiendo de cuantas cartas tengas, cambian de valor de 11 a 1 a medida que sacas mas cartas para evitar que te pases. El valor está determinado por el valor de las cartas que son menores a 12 con el A agregado si se reparte ten esto en cuenta mientras juegas cada juego y para cada mano que tengas.

Un crupier debe continuar sacando cartas después de su primera carta hasta que tenga 17 puntos o más, y se detiene. Esto significa que el crupier repartirá cartas automáticamente
hasta que tenga 17 puntos o más. Con 22 puntos o más, el crupier pierde. El jugador elige pedir o plantarse en cada carta extraída.



&nbsp;
##  Ejecución

En la terminal, sobre la carpeta del proyecto, correr el siguiente comando:

```
cabal run blackjack.cabal
```
