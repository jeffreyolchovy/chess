package com.olchovy.chess.domain

import com.olchovy.chess.domain.event._

case class Pawn(color: Piece.Color.Value, hasMoved: Boolean = false) extends Piece
{
  import Piece._

  val token = "p"

  def validate(attempt: MoveAttempt): Either[MoveFailure, Move] = try {
    val slope = attempt.board.slope(attempt.from, attempt.to).map(math.abs(_))
    val deltaY = attempt.to.y - attempt.from.y

    // Is the pawn's destination another piece?
    val isCapturing = attempt.capturedPiece.isDefined

    // If the pawn is capturing it can move diagonal, otherwise, it can move vertically
    val allowedSlope = if(isCapturing) Some(1d) else None

    // Pawns that aren't capturing and have not yet moved can move 2 squares, otherwise, 1
    val allowedDeltaY = if(isCapturing) 1 else if(hasMoved) 1 else 2

    attempt.test(slope == allowedSlope, "Pawns can move forward or diagonally when attacking")

    // White pawns can move upwards, black pawns can move downwards
    attempt.test(
      (attempt.piece.color == Color.white && deltaY < 0) ||
      (attempt.piece.color == Color.black && deltaY > 0),
      "Pawn can not move in that direction"
    )

    attempt.test(math.abs(deltaY) <= allowedDeltaY, "Move exceeds range of Pawn")

    // If the pawn has not yet moved, mark it
    val move = Move(attempt) { piece =>
      val pawn = piece.asInstanceOf[Pawn]
      if(!pawn.hasMoved) Pawn(pawn.color, hasMoved = true) else pawn
    }

    Right(move)
  } catch {
    case (e: MoveFailure) => Left(e)
    case (e: Throwable) => Left(MoveFailure(attempt, e.getMessage))
  }
}
