package com.olchovy.chess.domain.event

import com.olchovy.chess.domain._

case class MoveAttempt(board: ChessBoard, from: Board.Point, to: Board.Point) extends Move
{
  /* Validation routine that will either
   * (1) promote this attempt to a [successful] Move or
   * (2) decorate it with the error condition
   */
  def validate: Either[MoveFailure, Move] = try {
    // bounds checking
    test(board.contains(from) && board.contains(to), "Move is out of bounds")
    validateAttemptFrom
    validateAttemptTo
    delegate
  } catch {
    case (e: MoveFailure) => Left(e)
  }

  /* Assert that function `f` evaluates to true, or else throw MoveFailure with given error message */
  def test(f: => Boolean, error: String): Unit = if(!f) throw MoveFailure(this, error)

  /* Is the origin of the move attempt valid? */
  protected def validateAttemptFrom {
    // Was a piece chosen?
    test(piece != null, "You did not choose to move a piece")
    // Was a piece of the correct player chosen?
    test(piece.color == board.player, "Chosen piece to move is not yours")
  }

  /* Is the destination of the move attempt valid?
   * 
   * NOTE: Mixing in a 'Castling' trait on a MoveAttempt
   * can expose the functionality needed for adding the castling
   * feature of chess.
   */
  protected def validateAttemptTo {
    // If the destination is another piece, is it an opponent's piece?
    capturedPiece.foreach { capture =>
      test(capture.color == board.opponent, "Chosen destination is invalid")
    }
  }

  /* By default, after general validation is complete,
   * delegate validation to the piece that is to be moved,
   * as each piece contains logic for its individual idiosyncracies
   */
  protected def delegate: Either[MoveFailure, Move] = piece.validate(this)
}
