// DO NOT MODIFY FOR BASIC SUBMISSION
// scalastyle:off

package tetris.game

import java.awt.event
import java.awt.event.KeyEvent._

import engine.GameBase
import engine.graphics.{Color, Point, Rectangle}
import processing.core.{PApplet, PConstants}
import processing.event.KeyEvent
import tetris.game.TetrisGame._
import tetris.logic._

class TetrisGame extends GameBase {

  var gameLogic = TetrisLogic() //player 1 logic
  var gameLogic2 = TetrisLogic()
  val widthInPixels: Int = WidthCellInPixels * (gameLogic.nrColumns + 10) * 4 //more wide
  val heightInPixels: Int = HeightCellInPixels * gameLogic.nrRows * 2
  val updateTimer = new UpdateTimer(FramesPerSecond)
  val playerOneMiddle = Point(200, 360)
  val playerTwoMiddle = Point(810, 360)
  val screenArea: Rectangle = Rectangle(Point(0, 0), widthInPixels, heightInPixels)

  override def draw(): Unit = {
    if (gameLogic.isGameOver()) {
      drawGameOverScreen(playerOneMiddle)
      drawWinScreen(playerTwoMiddle)
    }
    else if (gameLogic2.isGameOver()) {
      drawGameOverScreen(playerTwoMiddle)
      drawWinScreen(playerOneMiddle)
    }
    else {
      background(5,5,5)
      updateState()
      drawGrid(gameLogic)
      drawGrid(gameLogic2, 610)
    }
  }

  def drawGameOverScreen(loseTextCoords: Point): Unit = {
    setFillColor(Color.Red)
    drawTextCentered("YOU LOSE!", 40, loseTextCoords)
  }

  def drawWinScreen(winTextCoords: Point): Unit = {
    setFillColor(Color.Green)
    drawTextCentered("YOU WIN!", size = 40, winTextCoords)
  }

  def drawGrid(tetrisLogic: TetrisLogic, offsetX: Int = 0): Unit = {
    val widthPerCell = screenArea.width / 2 / tetrisLogic.nrColumns - 20
    val heightPerCell = screenArea.height / tetrisLogic.nrRows

    for (y <- 0 until tetrisLogic.nrRows;
         x <- 0 until tetrisLogic.nrColumns) {
      drawCell(getCell(x, y, offsetX), tetrisLogic.getBlockAt(x, y), tetrisLogic)
    }
    for (x <- tetrisLogic.nrColumns + 1 until tetrisLogic.nrColumns + 5;
         y <- 0 until 4) {
      drawCell(getCell(x, y, offsetX), Background, tetrisLogic)
      if (x == tetrisLogic.nrColumns + 4) drawCell(getCell(tetrisLogic.upNextTetromino.coordArray(y).x, tetrisLogic.upNextTetromino.coordArray(y).y, offsetX), tetrisLogic.upNextTetromino.blockType, tetrisLogic)
    }

    def getCell(colIndex: Int, rowIndex: Int, offsetX: Int = 0): Rectangle = {
      val leftUp = Point(screenArea.left + offsetX + colIndex * widthPerCell, screenArea.top + rowIndex * heightPerCell)
      Rectangle(leftUp, widthPerCell, heightPerCell)
    }

    def drawCell(area: Rectangle, tetrisColor: TetrisBlock, tetrisLogic: TetrisLogic): Unit = {
      val color = tetrisBlockToColor(tetrisColor, tetrisLogic)
      setFillColor(color)
      drawRectangle(area)
    }

  }

  /** Method that calls handlers for different key press events.
    * You may add extra functionality for other keys here.
    * See [[event.KeyEvent]] for all defined keycodes.
    *
    * @param event The key press event to handle
    */
  override def keyPressed(event: KeyEvent): Unit = {

    event.getKeyCode match {
      case VK_Q           => gameLogic.rotateLeft()
      case VK_W           => gameLogic.rotateRight()
      case VK_A           => gameLogic.moveLeft()
      case VK_S           => gameLogic.moveDown()
      case VK_D           => gameLogic.moveRight()
      case VK_SPACE       => gameLogic.doHardDrop()
      case VK_UP          => gameLogic2.rotateRight()
      case VK_BACK_SLASH  => gameLogic2.rotateLeft()
      case VK_DOWN        => gameLogic2.moveDown()
      case VK_LEFT        => gameLogic2.moveLeft()
      case VK_RIGHT       => gameLogic2.moveRight()
      case VK_SHIFT       => gameLogic2.doHardDrop()
      case _ => ()
    }

  }

  override def settings(): Unit = {
    pixelDensity(displayDensity())
    size(widthInPixels, heightInPixels, PConstants.P2D)
  }

  override def setup(): Unit = {
    // Fonts are loaded lazily, so when we call text()
    // for the first time, there is significant lag.
    // This prevents it from happening during gameplay.
    text("", 0, 0)

    // This should be called last, since the game
    // clock is officially ticking at this point
    updateTimer.init()
  }

  def updateState(): Unit = {
    if (updateTimer.timeForNextFrame()) {
      gameLogic.moveDown()
      gameLogic2.moveDown()
      updateTimer.advanceFrame()
    }
  }

  def tetrisBlockToColor(color: TetrisBlock, tetrisLogic: TetrisLogic = gameLogic): Color =
    color match {
      case IBlock     => Color.LightBlue
      case OBlock     => Color.Yellow
      case LBlock     => Color.Orange
      case JBlock     => Color.Blue
      case SBlock     => Color.Green
      case Empty      => Color.Black
      case Background => Color.Background
      case TBlock     => Color.Purple
      case ZBlock     => Color.Red
      case GBlock     => Color.Ghost + tetrisBlockToColor(tetrisLogic.activeTetromino.blockType, tetrisLogic)
    }
}

object TetrisGame {

  val FramesPerSecond: Int = 2
  val WidthCellInPixels: Int = 15
  val HeightCellInPixels: Int = WidthCellInPixels

  def main(args:Array[String]): Unit = {
    PApplet.main("tetris.game.TetrisGame")
  }

}