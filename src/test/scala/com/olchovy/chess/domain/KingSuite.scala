package com.olchovy.chess.domain

import org.scalatest.FunSuite

class KingSuite extends FunSuite
{
  import Piece.Color._

  val board = ChessBoard(('d', 5) -> King(white))

  test("move up (1)") {
    assert(board.move(from = ('d', 5), to = ('d', 6)).isRight)
  }

  test("move up (2)") {
    assert(board.move(from = ('d', 5), to = ('d', 7)).isLeft)
  }

  test("move left (1)") {
    assert(board.move(from = ('d', 5), to = ('c', 5)).isRight)
  }

  test("move left (3)") {
    assert(board.move(from = ('d', 5), to = ('a', 5)).isLeft)
  }
}
