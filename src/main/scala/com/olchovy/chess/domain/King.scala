package com.olchovy.chess.domain

import com.olchovy.chess.domain.event._

case class King(color: Piece.Color.Value) extends Piece
{
  val token = "k"

  def validate(attempt: MoveAttempt): Either[MoveFailure, Move] = try {
    val deltaY = math.abs(attempt.to.y - attempt.from.y)
    val deltaX = math.abs(attempt.to.x - attempt.from.x)

    attempt.test(deltaY <= 1 && deltaX <= 1, "Move exceeds range of King")

    Right(attempt)
  } catch {
    case (e: MoveFailure) => Left(e)
    case (e: Throwable) => Left(MoveFailure(attempt, e.getMessage))
  }
}
