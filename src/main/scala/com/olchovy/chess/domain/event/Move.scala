package com.olchovy.chess.domain.event

import com.olchovy.chess.domain._

/* Model the movement of a piece to another square on the board */
trait Move
{
  import Piece._

  val board: ChessBoard
  
  val from: Board.Point
  
  val to: Board.Point

  lazy val piece: Piece = board(from).orNull

  lazy val capturedPiece: Option[Piece] = board(to)
}

object Move
{
  /* A factory function to promote a MoveAttempt to a Move
   * 
   * The update function allows the moved piece to alter itself,
   * enabling the ability to add the feature of 'promotion' to the program
   * or simply to maintain state on moved pieces (e.g. for 'en passant').
   */
  def apply(attempt: MoveAttempt)(update: Piece => Piece): Move = new Move {
    val board = attempt.board
    val from  = attempt.from
    val to    = attempt.to

    override lazy val piece = update(attempt.piece)
  }
}
