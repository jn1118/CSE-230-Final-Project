# CSE-230-Final-Project

Proposal for CSE230 final project

Collaborators: 
- Yiyan Chen, A16186093, github: YiyanChen; 
- Qipeng Xu, A59011253, github: QipengXu;
- Jiangnan Xu, A14534652, github: jn1118

We are going to build up a minesweeper game using Haskell, which is going to be displayed on the terminal and allow one users to play. 

Description:
The size of the board is n x n, n depends on the difficulty the user chooses.
Our game is a single-player puzzle video game. The objective of the game is to clear a square board containing hidden "mines" or bombs without detonating any of them, with help from clues about the number of neighbouring mines in each field. 

Rules of play: The game is played by revealing squares of the grid by clicking or otherwise indicating each square. If a square containing a mine is revealed, the player loses the game. If no mine is revealed, a digit is instead displayed in the square, indicating how many adjacent squares contain mines; if no mines are adjacent, the square becomes blank, and all adjacent squares will be recursively revealed. The player uses this information to deduce the contents of other squares and may either safely reveal each square or mark the square as containing a mine.

Basic functionalities we are going to implement:
1. Users can use those four keys mentioned above to move on the board and select which grid they want to reveal. Then, they can press d or f to reveal a mine or flag a bomb.

2. If a grid doesn't contain a bomb and it is reveal, a number will be shown to indicate how many adjacent bomb. If the number is 0, adjacent grid will be revealed **recursively**.

3. The game will record time a user uses. And a rank will be displayed to show all best records.

Further functionalities we are going to implement:
1. Reduced Uncertainty: the first grid chosen doesn't contain a bomn.
2. Use cursor to select the grid they want to reveal or add flag.

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
