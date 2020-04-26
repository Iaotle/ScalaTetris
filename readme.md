Tetris bonus submission details:

Display next blocks - drawGrid updated with an additional loop that draws 4x4 grid with the next block inside. nextTetromino + functions added to TetrisLogic.

Wall kick system - using https://harddrop.com/wiki/SRS wall kick data, the tetromino is offset to 4 different positions to try and find a spot where we can kick it to (in addition to original rotated position). This is done in the wallKickTetromino function.

Competitive 2-player Tetris - TetrisGame class updated to run 2 instances of TetrisLogic(), draw() updated to write GameOver and Win screens for players depending on the winner, drawGameOverScreen() and drawWinScreen() updated accordingly. drawGrid() updated with optional argument offsetX, which when passed to getCell allows to offset the board, in order to draw the 2nd player interface. width/height updated to be overall bigger, and double wide to accomodate 2nd player

Ghost tetromino - GBlock added, ghostTetromino functions added to TetrisLogic.

Graphics - Background color added, background set in Processing. Color updated to add + operator, which adds 1/4th of selected block color to the normally grey Ghost block.



How to run:
gradlew run