# Tic Tac Toe

This is a clojure example of Tic Tac Toe, the user can play tic tac toe against the Computer. Unfortunately, the user is not allowed to win. 
Don't you dare win. Don't. you. dare.

## Layout

All html/js/css files are located within the resources directory, I decided to use D3 to handle the shapes in javascript, and have a few jquery API calls to retrieve and store the game state

The Controller for the Application is located in the routes directory under src/connectfour/routes.

The models directory contains all of the inner workings of tictactoe <br />
matrix.clj - Code to display a game board<br />
validate.clj - Code to validate a board and determine winners <br />
playbrain.clj - Playing logic, for computer to play computer, or play random moves <br />
minmax.clj - Search tree using the basic minmax algorithm, uses clojure.zip<br />
alphabeta.clj - Alpha beta pruning version of the minmax algorithm<br />
existrecur.clj - My very first take on attempting to solve the problem, brute force method to solve<br />

I used luminus to create this project, which creates a easy scaffolding for creating small web apps, you can find the project site for this here. http://www.luminusweb.net/

## Prerequisites

You will need [Leiningen][1] 2.0 or above installed.

[1]: https://github.com/technomancy/leiningen

## Running

Start the Webserver by using

    lein run


The default port for the application will be 3000, but should you want to use another port just throw it after lein run as an argument.

   ex. lein run 4050


## License

Copyright © 2015 Joshua Cano
