package com.olchovy.chess.domain

import org.scalatest.FunSuite

class KnightSuite extends FunSuite
{
  import Piece.Color._

  val whiteKnight = Knight(white)

  val blackPawn = Pawn(black)

  // N @ e4, p @ f6
  val board = ChessBoard(('e', 4) -> whiteKnight, ('f', 6) -> blackPawn)

  test("move down (2), left (1)") {
    assert(board.move(from = ('e', 4), to = ('d', 2)).isRight)
  }

  test("move down (1), left (2)") {
    assert(board.move(from = ('e', 4), to = ('c', 3)).isRight)
  }

  test("move up (1), left (1)") {
    assert(board.move(from = ('e', 4), to = ('d', 3)).isLeft)
  }

  test("move up (1), right (2) for capture") {
    assert(board.move(from = ('e', 4), to = ('f', 6)).isRight)
  }
}

