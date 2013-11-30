connect4module
==============

A simple connect 4 module written in OCaml

The game board is represented as a list of lists where each inner list represents a row in the matrix (games should be 
be played on a minimum board of 4 inner lists each of which has length 4).

Init

The initial board is represented by a matrix of 0's. Note that the dimensions of the matrix can be arbitrary but they 
should be greater than for. Because the matrix is generalized, the the initial board must be built each time the game
is played. 

Moves

The move type is a number starting at 0 that represents which column the current player will place their piece in (player
1's pieces show up as 1's in the matrix, and player 2's show up as 2's. 

Win-checking

Checking is done systematically first by rows, then by columns, and then by diagonals. While row and column checking is 
trivial, diagonal checking is done by first checking the principal top-left to bottom-right diagonal, and then running 
the same function on the matrix where each of the sublists is reversed. If the matrix is a square matrix with dimensions
gretaer than 4 by 4, the top-right most square matrix is checked, this is done recursively until a matrix of dimension
4 by 4 is reached. At each iteration, the function checks the principal diagonal, and then checks the principal diagonal
of the reverse of this matrix. Then the entire procedure is run on the original matrix flipped horizontally, vertically
and then both horizontally and vertically. This results in a checking procedure that checks the top-left to bottom-right
and bottom-left to top-right diagonals of all the possible submatrices of the board at any given state.

If the board is a non-square matrix, either the left-most or top-most square submatrix is taken depending on whether the
height of the baord is greater than the width, or if the width of the board is greater than the height. Then the square 
diagonal checking procedure is run recursively on all possible square submatrices of the non-square matrix. This results
in every possible diagonal being checked at each step in the game. The game is over when one player achieves any possible
4-in a row. To check this, the procedure check4 is run on the list returned by either check-diagonals, check-rows, or 
check-columns. 

Potential for Increasing Efficiency

One way to make the checking procedures more efficient is to crop the board to the smallest possible rectangle conatining
all non-vacant entries at every turn after the 7th move (as this is the first time that either player can win the game.




