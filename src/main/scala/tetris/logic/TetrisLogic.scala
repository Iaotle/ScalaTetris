package tetris.logic

import engine.random.{RandomGenerator, ScalaRandomGen}
import tetris.game._
import tetris.logic.TetrisLogic._

import scala.collection.mutable.ArrayBuffer

/** To implement Tetris, complete the ``TODOs`` below.
  *
  * If you need additional files,
  * please also put them in the ``tetris`` package.
  */
class TetrisLogic(val randomGen: RandomGenerator,
                  val nrColumns: Int,
                  val nrRows: Int,
                  val initialBoard: Seq[Seq[TetrisBlock]]) {

  def this(random: RandomGenerator, nrColumns: Int, nrRows: Int) =
    this(random, nrColumns, nrRows, makeEmptyBoard(nrColumns, nrRows))

  def this() =
    this(new ScalaRandomGen(), DefaultWidth, DefaultHeight, makeEmptyBoard(DefaultWidth, DefaultHeight))

  case class Coordinate(x: Int, y: Int) {
    def +(rhs: Coordinate): Coordinate = Coordinate(x + rhs.x, y + rhs.y)
    def -(rhs: Coordinate): Coordinate = Coordinate(x - rhs.x, y - rhs.y)
  }

  val NrTetrominos: Int = 7
  val Left: Int = 1
  val Right: Int = -1
  val MiddleXPos: Int = (nrColumns / 2.0 - 1).ceil.toInt
  var rand: Int = 0
  var lockedTetrominos: Array[Array[TetrisBlock]] = Array.ofDim[TetrisBlock](nrColumns, nrRows)
  var activeTetromino: Tetromino = Tetromino()
  var upNextTetromino: Tetromino = Tetromino()
  var ghostTetromino: Tetromino = Tetromino()

  startGame()

  def startGame(): Unit = {
    initBoard()
    updateRandomGen()
    newTetromino()
    newGhostTetromino()
    updateGhostTetromino()
    updateRandomGen()
    newUpNextTetromino()
  }

  def updateGhostTetromino(): Unit = {
    resetGhostTetromino()
    while (!isLocked(ghostTetromino)) moveTetromino(0, 1, ghostTetromino)
  }

  def resetGhostTetromino(): Unit = for (i <- ghostTetromino.coordArray.indices) ghostTetromino.coordArray(i) = activeTetromino.coordArray(i)

  def newGhostTetromino(): Unit = {
    ghostTetromino = Tetromino()
    initTetromino(ghostTetromino, rand)
    updateGhostTetromino()
  }

  def isLocked(tetromino: Tetromino = activeTetromino): Boolean = {
    for (i <- tetromino.coordArray.indices) {
      if (tetromino.coordArray(i).y == nrRows - 1) return true //are we on the ground
      else if (lockedTetrominos(tetromino.coordArray(i).x)(tetromino.coordArray(i).y + 1) != Empty) return true // or on top of other blocks
    }
    false
  }

  def initBoard(): Unit = for (x <- 0 until nrColumns; y <- 0 until nrRows) lockedTetrominos(x)(y) = initialBoard(y)(x)

  def rotateLeft(): Unit = if (!isGameOver() & activeTetromino.blockType != OBlock) rotateTetromino(Left)

  def rotateRight(): Unit = if (!isGameOver() & activeTetromino.blockType != OBlock) rotateTetromino(Right)

  def moveLeft(): Unit = if (!isGameOver()) moveSideways(Left)

  def moveSideways(direction: Int): Unit = {
    moveTetromino(-direction, 0)
    if (isGameOver()) {
      moveTetromino(direction, 0)
    }
    else updateGhostTetromino()
  }

  def moveTetromino(x: Int, y: Int, tetromino: Tetromino = activeTetromino): Unit = {
    for (i <- tetromino.coordArray.indices) tetromino.coordArray(i) += Coordinate(x, y)
    tetromino.anchorPoint = tetromino.coordArray(0)
  }

  def isGameOver(tetromino: Tetromino = activeTetromino): Boolean = {
    for (i <- tetromino.coordArray.indices) {
      if (!((0 until nrColumns contains tetromino.coordArray(i).x) & (0 until nrRows contains tetromino.coordArray(i).y))) return true //in bounds
      else if (lockedTetrominos(tetromino.coordArray(i).x)(tetromino.coordArray(i).y) != Empty) return true //check if tetromino blocks are taken
    }
    false
  }

  def moveRight(): Unit = if (!isGameOver()) moveSideways(Right)

  def doHardDrop(): Unit = {
    while (!isLocked()) moveDown()
    moveDown()
  }

  def updateRandomGen(): Unit = rand = randomGen.randomInt(NrTetrominos)

  def newUpNextTetromino(): Unit = {
    upNextTetromino = Tetromino()
    initTetromino(upNextTetromino, rand)
    moveTetromino(MiddleXPos+4, 1, upNextTetromino)
  }

  def moveDown(): Unit = {
    if (!isGameOver()) {
      if (isLocked()) {
        lockTetromino()
        checkLineClear()
        newTetromino()
        newGhostTetromino()
        updateGhostTetromino()
        updateRandomGen()
        newUpNextTetromino()
      }
      else moveTetromino(0, 1)
    }
  }

  def checkLineClear(): Unit = {
    var fullLineCounter: Int = 0
    for (y <- 0 until nrRows; x <- 0 until nrColumns) {
      if (x == 0) fullLineCounter = 0
      if (lockedTetrominos(x)(y) != Empty) fullLineCounter += 1
      if (fullLineCounter == nrColumns) lineClear(y)
    }
  }

  def lineClear(lineNumber: Int): Unit = for (y <- lineNumber to 1 by -1; x <- 0 until nrColumns) lockedTetrominos(x)(y) = lockedTetrominos(x)(y - 1)

  def lockTetromino(): Unit = for (i <- activeTetromino.coordArray.indices) lockedTetrominos(activeTetromino.coordArray(i).x)(activeTetromino.coordArray(i).y) = activeTetromino.blockType

  def newTetromino(): Unit = {
    activeTetromino = Tetromino()
    initTetromino(activeTetromino, rand)
  }

  def wrapRotation(i: Int, tetromino: Tetromino): Unit = {
    tetromino.rotation += i
    if (tetromino.rotation == -1) tetromino.rotation = 3
    else if (tetromino.rotation == 4) tetromino.rotation = 0
  }

  def initTetromino(tetromino: Tetromino, block: Int): Unit ={
    block match {
      case 0 =>
        tetromino.blockType = IBlock
        tetromino.coordArray = ArrayBuffer(tetromino.anchorPoint, tetromino.anchorPoint + Coordinate(-1, 0), tetromino.anchorPoint + Coordinate(1, 0), tetromino.anchorPoint + Coordinate(2, 0))
      case 1 =>
        tetromino.blockType = JBlock
        tetromino.coordArray = ArrayBuffer(tetromino.anchorPoint, tetromino.anchorPoint + Coordinate(-1, -1), tetromino.anchorPoint + Coordinate(-1, 0), tetromino.anchorPoint + Coordinate(1, 0))
      case 2 =>
        tetromino.blockType = LBlock
        tetromino.coordArray = ArrayBuffer(tetromino.anchorPoint, tetromino.anchorPoint + Coordinate(-1, 0), tetromino.anchorPoint + Coordinate(1, 0), tetromino.anchorPoint + Coordinate(1, -1))
      case 3 =>
        tetromino.blockType = OBlock
        tetromino.coordArray = ArrayBuffer(tetromino.anchorPoint, tetromino.anchorPoint + Coordinate(0, -1), tetromino.anchorPoint + Coordinate(1, -1), tetromino.anchorPoint + Coordinate(1, 0))
      case 4 =>
        tetromino.blockType = SBlock
        tetromino.coordArray = ArrayBuffer(tetromino.anchorPoint, tetromino.anchorPoint + Coordinate(-1, 0), tetromino.anchorPoint + Coordinate(0, -1), tetromino.anchorPoint + Coordinate(1, -1))
      case 5 =>
        tetromino.blockType = TBlock
        tetromino.coordArray = ArrayBuffer(tetromino.anchorPoint, tetromino.anchorPoint + Coordinate(-1, 0), tetromino.anchorPoint + Coordinate(0, -1), tetromino.anchorPoint + Coordinate(1, 0))
      case 6 =>
        tetromino.blockType = ZBlock
        tetromino.coordArray = ArrayBuffer(tetromino.anchorPoint, tetromino.anchorPoint + Coordinate(-1, -1), tetromino.anchorPoint + Coordinate(0, -1), tetromino.anchorPoint + Coordinate(1, 0))
    }
  }

  def offsetTetromino(direction: Int, tetromino: Tetromino): Unit = {
    if (direction == -1) wrapRotation(-1, tetromino)
    tetromino.rotation match {
      case 0 => moveTetromino(0, 1, tetromino)
      case 1 => moveTetromino(-1, 0, tetromino)
      case 2 => moveTetromino(0, -1, tetromino)
      case 3 => moveTetromino(1, 0, tetromino)
    }
    if (direction == -1) wrapRotation(1, tetromino)
  }

  def wallKickTetromino(d: Int, tetromino: Tetromino): Unit ={
    val direction = -d
    if (tetromino.blockType == IBlock){
      tetromino.rotation match {
      case 0 =>
        moveTetromino(-2*direction, 0, tetromino)
        if (isGameOver()) {
          moveTetromino(3*direction, 0, tetromino)
          if (isGameOver()) {
            moveTetromino(-3*direction, -1*direction, tetromino)
            if (isGameOver()) {
              moveTetromino(3*direction, 3*direction, tetromino)
              if (isGameOver()) moveTetromino(-1*direction, -2*direction, tetromino)
            }
          }
        }
      case 1 =>
        moveTetromino(-1*direction, 0, tetromino)
        if (isGameOver()) {
          moveTetromino(3*direction, 0, tetromino)
          if (isGameOver()) {
            moveTetromino(-3*direction, 2*direction, tetromino)
            if (isGameOver()) {
              moveTetromino(3*direction, -3*direction, tetromino)
              if (isGameOver()) moveTetromino(-2*direction, 1*direction, tetromino)
            }
          }
        }
      case 2 =>
        moveTetromino(2*direction, 0, tetromino)
        if (isGameOver()){
          moveTetromino(-3*direction, 0, tetromino)
          if (isGameOver()){
            moveTetromino(3*direction, 1*direction, tetromino)
            if (isGameOver()){
              moveTetromino(-3*direction, -3*direction, tetromino)
              if (isGameOver()) moveTetromino(1*direction, 2*direction, tetromino)
            }
          }
        }
      case 3 =>
        moveTetromino(1*direction, 0, tetromino)
        if (isGameOver()) {
          moveTetromino(-3*direction, 0, tetromino)
          if (isGameOver()) {
            moveTetromino(3*direction, -2*direction, tetromino)
            if (isGameOver()) {
              moveTetromino(-3*direction, 3*direction, tetromino)
              if (isGameOver()) moveTetromino(2*direction, -1*direction, tetromino)
            }
          }
        }
      }
    }
    else {
      tetromino.rotation match {
      case 0 =>
        moveTetromino(-1*direction, 0, tetromino)
        if (isGameOver()) {
          moveTetromino(0, 1*direction, tetromino)
          if (isGameOver()) {
            moveTetromino(1*direction, -3*direction, tetromino)
            if (isGameOver()) {
              moveTetromino(-1*direction, 0, tetromino)
              if (isGameOver()) moveTetromino(1*direction, 2*direction, tetromino)
            }
          }
        }
      case 1 =>
        moveTetromino(1*direction, 0, tetromino)
        if (isGameOver()) {
          moveTetromino(0, -1*direction, tetromino)
          if (isGameOver()) {
            moveTetromino(-1*direction, 3*direction, tetromino)
            if (isGameOver()) {
              moveTetromino(1*direction, 0, tetromino)
              if (isGameOver()) moveTetromino(-1*direction, -2*direction, tetromino)
            }
          }
        }
      case 2 =>
        moveTetromino(1*direction, 0, tetromino)
        if (isGameOver()){
          moveTetromino(0, 1*direction, tetromino)
          if (isGameOver()){
            moveTetromino(-1*direction, -3*direction, tetromino)
            if (isGameOver()){
              moveTetromino(1*direction, 0, tetromino)
              if (isGameOver()) moveTetromino(-1*direction, 2*direction, tetromino)
            }
          }
        }
      case 3 =>
        moveTetromino(-1*direction, 0, tetromino)
        if (isGameOver()) {
          moveTetromino(0, -1*direction, tetromino)
          if (isGameOver()) {
            moveTetromino(1*direction, 3*direction, tetromino)
            if (isGameOver()) {
              moveTetromino(-1*direction, 0, tetromino)
              if (isGameOver()) moveTetromino(1*direction, -2*direction, tetromino)
            }
          }
        }
      }
    }
  }

  def rotateTetromino(direction: Int, tetromino: Tetromino = activeTetromino): Unit ={
    for (i <- 1 until tetromino.coordArray.size) {
      tetromino.coordArray(i) -= tetromino.anchorPoint
      tetromino.coordArray(i) = Coordinate((0 * tetromino.coordArray(i).x) + (1 * direction * tetromino.coordArray(i).y), (-1 * direction * tetromino.coordArray(i).x) + (0 * tetromino.coordArray(i).x)) //apply rotation (dot product with rotation matrix)
      tetromino.coordArray(i) += tetromino.anchorPoint
    }
    if (tetromino.blockType == IBlock) offsetTetromino(direction, tetromino)
    if (isGameOver(tetromino) & tetromino.blockType != OBlock) wallKickTetromino(direction, tetromino)
    wrapRotation(-direction, tetromino)
    if (isGameOver(tetromino)) rotateTetromino(-direction, tetromino)
    updateGhostTetromino()
  }

  def getBlockAt(x: Int, y: Int): TetrisBlock = {
    if (activeTetromino.coordArray.contains(Coordinate(x, y))) activeTetromino.blockType
    else if (ghostTetromino.coordArray.contains(Coordinate(x, y))) GBlock
    else lockedTetrominos(x)(y)
  }

  case class Tetromino() {
    var rotation: Int = 0
    var blockType: TetrisBlock = IBlock
    var anchorPoint: Coordinate = Coordinate(MiddleXPos, 1)
    var coordArray: ArrayBuffer[Coordinate] = ArrayBuffer()
  }
}

object TetrisLogic {

  def makeEmptyBoard(nrColumns: Int, nrRows: Int): Seq[Seq[TetrisBlock]] = {
    val emptyLine = Seq.fill(nrColumns)(Empty)
    Seq.fill(nrRows)(emptyLine)
  }

  val DefaultWidth: Int = 10
  val NrTopInvisibleLines: Int = 4
  val DefaultVisibleHeight: Int = 20
  val DefaultHeight: Int = DefaultVisibleHeight + NrTopInvisibleLines


  def apply() = new TetrisLogic(new ScalaRandomGen(),
                                DefaultWidth,
                                DefaultHeight,
                                makeEmptyBoard(DefaultWidth, DefaultHeight))

}