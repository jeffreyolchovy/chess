package com.olchovy.chess.domain

import com.olchovy.chess.domain.event._

case class Bishop(color: Piece.Color.Value) extends Piece with Piece.ValidationHelper
{
  val token = "b"

  /* Assert that move is diagonal and path of move is not obstructed */
  def validate(attempt: MoveAttempt): Either[MoveFailure, Move] = try {
    testDirection(attempt)(diagonal)
    testPath(attempt)
    Right(attempt)
  } catch {
    case (e: MoveFailure) => Left(e)
    case (e: Throwable) => Left(MoveFailure(attempt, e.getMessage))
  }
}
