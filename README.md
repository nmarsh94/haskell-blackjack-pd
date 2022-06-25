# Trabajo final - Programación Declarativa

## BlackJack en Haskell

&nbsp;

## Tema del proyecto
Se desarrolló un programa que permite al usuario jugar al juego de cartas **BlackJack**.
El juego ha sido implementado en Haskell.

&nbsp;

##   Definición

Este juego consiste en enfrentarse de forma individual a la banca comparando su mano contra la de la banca o crupier, intentando conseguir 21 puntos o el número más cercano posible sin pasarse.
Para conseguir dicha puntuación se suman los valores de dos cartas que se reparten al inicio, con los de las nuevas cartas que, opcionalmente, se podrán añadir si el jugador pide otra carta. Si las dos cartas iniciales suman 21, se denomina Blackjack, y es la mejor jugada. Cuando un jugador no suma 21 con sus dos cartas podrá pedir cartas para conseguir dicho número o uno cercano, pero si el jugador se pasa de esos 21 puntos pierde, indistintamente de lo que haga la banca.
La banca también juega, pero tiene unas reglas muy definidas que se han de tener en cuenta. Si la suma de las cartas de la banca es 16 o menos, debe pedir carta y si suman 17 o más se debe plantar.

&nbsp;
##  Ejecución

En la terminal, sobre la carpeta del proyecto, correr el siguiente comando:
Correr los siguientes comandos en la carpeta raíz del proyecto:

```
cabal run blackjack.cabal
```
