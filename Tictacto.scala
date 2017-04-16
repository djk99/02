import scala.io.StdIn

class Board(marks: Seq[Char]) {

  import Board._

  def humanMoves(pos: Int) = Board(marks.updated(pos, HUMAN))

  def availablePosIdxs = for ((c, i) <- marks.zipWithIndex if c != HUMAN && c != COMPUTER) yield i

  def computerMoves = {
    val randomPos = availablePosIdxs(RANDOM_GEN.nextInt(availablePosIdxs.length))
    val updatedMarks = marks.updated(randomPos, COMPUTER)
    Board(updatedMarks)
  }

  def isWinner(w: Char) = WIN_LINES.exists { case (i, j, k) => marks(i) == w && marks(j) == w && marks(k) == w }

  def isTie = marks.forall(c => c == HUMAN || c == COMPUTER)

  def isOver = isWinner(COMPUTER) || isWinner(HUMAN) || isTie

  def printBoard {
    marks.grouped(BOARD_LENGTH).foreach(r => println(r(0) + " " + r(1) + " " + r(2)))
  }

  def printWinner {
    if (isWinner(HUMAN)) println("You win.")
    else if (isWinner(COMPUTER)) println("Computer wins.")
    else println("It's a tie.")
  }
}

object Board {
  val HUMAN = 'X'
  val COMPUTER = 'Q'
  val START_BOARD = ('0' to '8').toSeq
  val BOARD_LENGTH = 3
  val WIN_LINES = Seq((0, 1, 2), (3, 4, 5), (6, 7, 8), (0, 3, 6), (1, 4, 7), (2, 5, 8), (0, 4, 8), (2, 4, 6))
  val RANDOM_GEN = new util.Random(System.currentTimeMillis)

  def apply(marks: Seq[Char]) = new Board(marks)
}


object TicTacToe {

  import Board._

  def readValidMove(board: Board): Int = {
    print("Choose a move: ")
    val move = StdIn.readInt
    if (board.availablePosIdxs.contains(move)) {
      move
    } else {
      println("Invalid move. Choose another move in " + board.availablePosIdxs)
      readValidMove(board)
    }
  }

  def play(board: Board, turn: Char) {

    board.printBoard
    if (board.isOver) {
      board.printWinner
      return
    }

    if (turn == HUMAN) {
      val nextBoard = board.humanMoves(readValidMove(board))
      play(nextBoard, COMPUTER)
    } else {
      println("And computer plays:")
      val nextBoard = board.computerMoves
      play(nextBoard, HUMAN)
    }
  }

  def main(args: Array[String]): Unit = {
    play(new Board(START_BOARD), HUMAN)
  }
}
