package com.olchovy.chess.domain

import org.scalatest.FunSuite

class KingSuite extends FunSuite
{
  val piece = King(Piece.Color.white)

  // K @ d5
  val board = new ChessBoard(Seq.fill(27)(None) ++ Seq(Some(piece)) ++ Seq.fill(36)(None))

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

