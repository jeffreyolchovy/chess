package com.olchovy.chess.domain

import com.olchovy.chess.domain.event._

case class Rook(color: Piece.Color.Value) extends Piece with Piece.ValidationHelper
{
  val token = "r"

  /* Assert that the move is vertical or horizontal and that the path is not obstructed */
  def validate(attempt: MoveAttempt): Either[MoveFailure, Move] = try {
    testDirection(attempt)(vertical ++ horizontal)
    testPath(attempt)
    Right(attempt)
  } catch {
    case (e: MoveFailure) => Left(e)
    case (e: Throwable) => Left(MoveFailure(attempt, e.getMessage))
  }
}
