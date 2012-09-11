package com.olchovy.chess.domain

import com.olchovy.chess.domain.event._

case class Knight(color: Piece.Color.Value) extends Piece
{
  val token = "n"

  def validate(attempt: MoveAttempt): Either[MoveFailure, Move] = try {
    val deltaY = math.abs(attempt.to.y - attempt.from.y)
    val deltaX = math.abs(attempt.to.x - attempt.from.x)

    attempt.test((deltaY == 2 && deltaX == 1) || (deltaY == 1 && deltaX == 2), "Move exceeds range of Knight")

    Right(attempt)
  } catch {
    case (e: MoveFailure) => Left(e)
    case (e: Throwable) => Left(MoveFailure(attempt, e.getMessage))
  }
}
