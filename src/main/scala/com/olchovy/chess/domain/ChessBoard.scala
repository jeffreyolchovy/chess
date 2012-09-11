package com.olchovy.chess.domain

/* A board of fixed size (8 x 8), that can hold pieces on any of its squares */
case class ChessBoard(override val state: Seq[Option[Piece]], turn: Int = 0) extends Board[Piece](8, 8, state)
{
  import Piece._

  /* The player that owns the current turn */
  lazy val player: Color.Value = if(turn % 2 == 0) Color.white else Color.black

  /* The player's opponent */
  lazy val opponent: Color.Value = Color(player.id ^ 1)

  lazy val (playerName, opponentName) = player match {
    case Piece.Color.white => ("white", "black")
    case Piece.Color.black => ("black", "white")
  }

  /* A map of the player's pieces */
  lazy val playerPieces: Map[Board.Point, Piece] = pieces(player)

  /* A map of the opponent's pieces */
  lazy val opponentPieces: Map[Board.Point, Piece] = pieces(opponent)

  /* The position of the player's King (if available) */
  lazy val positionOfKing: Option[Board.Point] = playerPieces.find(_._2.isInstanceOf[King]).map(_._1)  

  /* A map of the opponent's pieces that have placed the King in check */
  lazy val threatsToKing: Map[Board.Point, Piece] = positionOfKing.map { target =>
    opponentPieces.filter {
      // can this piece reach the King? (this will not test for subsequent checks)
      case (source, piece) => copy(turn = turn + 1).move(source, target, testInCheck = false).isRight
    }
  }.getOrElse(Map.empty[Board.Point, Piece])

  /* Can any of the opponent's pieces reach the King? */
  lazy val isInCheck: Boolean = !threatsToKing.isEmpty

  /* Is the player in an inescapable state check? */
  lazy val isInCheckmate: Boolean = isInCheck && positionOfKing.map { case kingPoint @ Board.Point(x, y) =>
    val Some(piece) = apply(kingPoint)
    val king = piece.asInstanceOf[King]
    val adjacentPoints = {
      val xs = for(i <- -1 to 1) yield x + i
      val ys = for(i <- -1 to 1) yield y + i
      for(x <- xs; y <- ys) yield Board.Point(x, y)
    }.filter(point => point != kingPoint && contains(point))

    val kingMoveAttempts = adjacentPoints.map(move(kingPoint, _))
    val kingMove = kingMoveAttempts.reduceLeft((a, b) => if(b.isRight) b else a)
    val kingCanMove = kingMove.isRight

    // if king can move to an adjacent sqaure, we're not in checkmate
    if(kingCanMove) 
      false
    // if there is more than one threat we can not defend against it in one turn
    else if(threatsToKing.size > 1)
      true
    // if any of our pieces can capture or block the path of the threat, we're not in checkmate
    else playerPieces.map { case (point, piece) =>
      val threatPoint = threatsToKing.keys.head
      lazy val canCapture = move(point, threatPoint).isRight
      lazy val canBlock = {
        val attempts = path(threatPoint, kingPoint).init.map(move(point, _))
        attempts.reduceLeft((a, b) => if(b.isRight) b else a).isRight
      }

      piece match {
        case Bishop(_) | Rook(_) | Queen(_) => canCapture || canBlock
        case _ => canCapture
      }
    }.find(_ == true).isEmpty
  }.getOrElse(false)

  /* Retrieve a map of pieces for the given color */
  def pieces(color: Color.Value): Map[Board.Point, Piece] = Map(state.zipWithIndex.flatMap {
    case (Some(piece), i) if piece.color == color => Some(Board.Point(i % numCols, i / numCols) -> piece)
    case _ => None
  }: _*)

  import event._

  /* The publicly accessible move function, addressable by (char, int) pairs */
  def move(from: (Char, Int), to: (Char, Int)): Either[Throwable, ChessBoard] = {
    move(point(from._1, from._2), point(to._1, to._2))
  }

  /* Move a piece from 'from' to 'to'.
   *
   * If the move is valid, execute it and return a new, updated board.
   * 
   * By default, if the current player was in a state of check, fail if the move does not remove him from check.
   */
  private def move(from: Board.Point, to: Board.Point, testInCheck: Boolean = true): Either[Throwable, ChessBoard] = {
    val attempt = MoveAttempt(this, from, to)

    attempt.validate match {
      case Left(e) => Left(e)
      case Right(move) => 
        val newState = state.zipWithIndex.collect {
          // swap target with source
          case (_, i) if i == indexOf(move.to) => Some(move.piece)

          // swap source with nothing
          case (_, i) if i == indexOf(move.from) => None

          // no-op
          case (o, _) => o
        }

        // create new theoretical board (with same turn) to test if state is fully valid
        val newBoard = ChessBoard(newState, turn = turn)

        if(testInCheck && isInCheck && newBoard.isInCheck)
          Left(MoveFailure(attempt, "Attempted move did not escape check"))
        else if(testInCheck && newBoard.isInCheck)
          Left(MoveFailure(attempt, "Attempted move voluntarily places player in check"))
        else
          Right(newBoard.copy(turn = turn + 1))
    }
  }

  def prettyPrint {
    def indent(n: Int) = " " * n
    val colHeaders = Stream.from('a').take(numCols).map(i => " %c ".format(i.toChar)).mkString("")

    // print top column address headers
    println("\n" + indent(3) + colHeaders + "\n")

    for(j <- 0 until numRows) {
      val row = numRows - j

      for(col <- 'a' to 'h') {
        // print left row address headers
        if(col == 'a') print("%d%s".format(row, indent(2)))
        
        // print value of state at address (col, row)
        print(apply(col, row).map(p => " %s ".format(p.toString)).getOrElse(" □ "))

        // print right row address headers
        if(col == 'h') print("%s%d".format(indent(2), row))
      }

      // break line
      println()
    }

    // print bottom column address headers
    println("\n" + indent(3) + colHeaders + "\n")
  }
}

object ChessBoard
{
  import Piece._

  /* Create a board with no pieces */
  def empty: ChessBoard = ChessBoard(Seq.fill(64)(None))

  /* Create a board for a new chess game (black on top, white on bottom)*/
  def apply(): ChessBoard = {
    val blackPieces: Seq[Piece] = {
      val color = Color.black
      Seq(Rook(_), Knight(_), Bishop(_), Queen(_), King(_), Bishop(_), Knight(_), Rook(_)).map(_.apply(color)) ++
      Seq.fill(8)(Pawn(color))
    }

    val whitePieces: Seq[Piece] = {
      val color = Color.white
      Seq.fill(8)(Pawn(color)) ++
      Seq(Rook(_), Knight(_), Bishop(_), Queen(_), King(_), Bishop(_), Knight(_), Rook(_)).map(_.apply(color))
    }

    ChessBoard(blackPieces.map(Option(_)) ++ Seq.fill(32)(None) ++ whitePieces.map(Option(_)))
  }

  /* Create a board with an initial state as defined by (coordinate, piece) pairs */
  def apply(mappings: ((Char, Int), Piece)*): ChessBoard = {
    val board = empty
    val map = Map(mappings.toSeq.map { case ((col, row), piece) => board.indexOf(board.point(col, row)) -> piece }: _*)
    val newState = board.state.zipWithIndex.collect { case (_, index) => map.get(index) }

    ChessBoard(newState)
  }
}
