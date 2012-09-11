package com.olchovy.chess.domain

import com.olchovy.chess.domain.event._

/* A chess piece */
trait Piece
{
  import Piece._

  val color: Color.Value

  val token: String

  /* Can the piece legally make the given move? */
  def validate(move: MoveAttempt): Either[MoveFailure, Move]

  override def toString = if(color == Color.white) token.capitalize else token
}

object Piece
{
  object Color extends Enumeration {
    val white = Value(0)
    val black = Value(1)
  }

  /* Mixin for pieces that allow a sliding range of movements (e.g. bishop, rook, queen)
   *
   * Contains tests for verifying the direction of an attempted move
   * and for determining if the path of the attempted move is obstructed.
   */
  trait ValidationHelper
  {
    this: Piece =>

    import collection.BitSet

    protected final val vertical = BitSet(0)

    protected final val horizontal = BitSet(1)

    protected final val diagonal = BitSet(2)

    /* @throws MoveFailure */
    def testDirection(attempt: MoveAttempt)(directions: BitSet) {
      val direction = attempt.board.slope(attempt.from, attempt.to) match {
        case None => 0
        case Some(0d) => 1
        case Some(n) if math.abs(n) == 1d => 2
        case _ => 3
      }

      attempt.test(directions.contains(direction), "Invalid direction of movement for selected piece")
    }

    /* @throws MoveFailure */
    def testPath(attempt: MoveAttempt) {
      val path = attempt.board.path(attempt.from, attempt.to)
      attempt.test(!path.isEmpty, "Invalid move")
      attempt.test(path.init.flatMap(attempt.board(_)).isEmpty, "Path of desired move is blocked")
    }
  }
}
