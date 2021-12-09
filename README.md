# CSE-230-Final-Project

Proposal for CSE230 final project

Collaborators:

- Yiyan Chen, A16186093, github: YiyanChen;
- Qipeng Xu, A59011253, github: QipengXu;
- Jiangnan Xu, A14534652, github: jn1118

We are going to build up a Mamono game using Haskell, which is going to be displayed on the terminal and allow one users to play.

Description:
The size of the board is n x n, n depends on the difficulty the user chooses.
Our game is a single-player terminal game. Player gains levels by killing weak monsters and win when the user defeat them all. The objective of the game is to clear a square board containing hidden "mines" or bombs without detonating any of them, with help from clues about the number of neighbouring mines in each field.

Rules of play: The n x n board is divided into cells, with monsters (from level 1 to level 5) randomly distributed. The number on a cell shows the sum of the level of monsters adjacent to it (there are at most 8 monsters around one cell). Using this information, player can determine which cells are safe or contain mosters that the user is able to beat. The player has Level, HP, EX, and those information will display next to the board. Player can only beat monsters with lower or equal level. If a player successfully beats a monster, he/she will gain experience. Otherwise, he/she will lost HP according to the level of the monster. The player will increase level if he/she achieves the required experience for next level. The game will end if the player's HP is lower than 0. To win, the player need to defeat all monsters.

Basic functionalities we are going to implement:

1. Users can use those four keys mentioned above to move on the board and select which grid they want to reveal. Then, they can press d to "open" that cell.

2. If a grid doesn't contain a monster and it is revealed, the sum of the level of monsters adjacent to it will be shown. If the number is 0, adjacent grid will be revealed **recursively**.

3. The game will record the player's HP, EX, and level, and put all the information next to the board.

Further functionalities we are going to implement:

1. Generate board and randomly put the certain amount of monsters with different level on the board.

Library we are going to use: Brick

# Milestone 2

1. application architecture (the key components):

The application consists of 3 main components: IO component, game component, UI component.

- IO component: responsible for loading files and save files
- Game component: including some data types (e.g. grid, board) and some logic functions (e.g. click, flag, rank, save...)
- UI component: responsible for rendering user interface(using brick).

2. Challenges met so far and solutions:

The group members are not familiar enough with Haskell, so it is difficult to get started with brick. Therefore, we start with the user guide of brick and a youtube video tutorial to help our group get familiar with brick.
As the end of the quarter approaches, the team members’ schedule is very tight. Therefore everyone needs to have a clear timeline for completing their tasks to ensure progress.

3. Do we expect to meet our goals until the deadline?

Not sure. If the time not allowed, we may give up the “undo” functionality.
