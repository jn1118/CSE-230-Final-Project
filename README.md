# CSE-230-Final-Project

Proposal for CSE230 final project

Collaborators: 
- Yiyan Chen, A16186093, github: YiyanChen; 
- Qipeng Xu, A59011253, github: QipengXu;
- Jiangnan Xu, A14534652, github: jn1118

We are going to build up a Gomoku game using Haskell, which is going to be displayed on the terminal and allow two users to play simultaneously. 

Description:
The size of the board is 15 x 15, which is empty initially.
Our game allows two players to play together, one uses black stone, which is controlled by the up, down, left, right on the keyboard. The other user uses white stones, which are controlled by the W, S, A, D on the keyboard. 

Rules of play: Players alternate turns placing a stone of their color on an empty intersection. Black goes first. The winner is the first player to form an unbroken chain of five stones horizontally, vertically, or diagonally. 

Basic functionalities we are going to implement:
1. Users can use those four keys mentioned above to move on the board and select which place they want to put their stones. Then, they can press shift(for black stone) or return(for white stone) to put the stones on the intersections using. 
2. When the first player forms an unbroken chain of five stones horizontally, vertically, or diagonally, there will be a pop-up window announcing who is the winner and the game is over.
3. There will be a reset button that will reset the board to empty, which will also restart the game.

Further functionalities we are going to implement:
1. Use cursor to select the place they want to put the stones
2. Give user a hint (how to place the stone) or a warning that the opponent is going to win
3. Undo the table to the last status (ask for two players’ confirmation)

Library we are going to use: Brick

# Milestone 2

1. application architecture (the key components):
The application consists of 3 main components:IO component, game component, UI component.
- IO component: responsible for loading files and save files
- Game component: including some data types (e.g. stone,cell, board) and some logic functions (e.g. moveStone, putStone, checkWinner, giveHint …) 
- UI component: responsible for rendering user interface(using brick).

2. Challenges met so far and solutions:
The group members are not familiar enough with Haskell, so it is difficult to get started with brick. Therefore, we start with the user guide of brick and a youtube video tutorial to help our group get familiar with brick.
As the end of the quarter approaches, the team members’ schedule is very tight. Therefore everyone needs to have a clear timeline for completing their tasks to ensure progress.

3. Do we expect to meet our goals until the deadline?
Not sure. If the time not allowed, we may give up the “undo” functionality.
