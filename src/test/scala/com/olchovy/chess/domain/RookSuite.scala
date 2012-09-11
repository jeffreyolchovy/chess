package com.olchovy.chess.domain

import org.scalatest.FunSuite

class RookSuite extends FunSuite
{
  import Piece.Color._

  val piece = Rook(white)

  val whiteObstacle = Pawn(white)

  val blackObstacle = Pawn(black)

  // R @ d5, P @ b5, p @ g5
  val board = new ChessBoard(
    Seq.fill(25)(None) ++
    Seq(Some(whiteObstacle)) ++
    Seq.fill(1)(None) ++
    Seq(Some(piece)) ++
    Seq.fill(2)(None) ++
    Seq(Some(blackObstacle)) ++
    Seq.fill(33)(None)
  )

  test("move up") {
    assert(board.move(from = ('d', 5), to = ('d', 8)).isRight)
  }

  test("move down") {
    assert(board.move(from = ('d', 5), to = ('d', 2)).isRight)
  }

  test("move left") {
    assert(board.move(from = ('d', 5), to = ('c', 5)).isRight)
    assert(board.move(from = ('d', 5), to = ('b', 5)).isLeft)
    assert(board.move(from = ('d', 5), to = ('a', 5)).isLeft)
  }

  test("move right") {
    assert(board.move(from = ('d', 5), to = ('f', 5)).isRight)
    assert(board.move(from = ('d', 5), to = ('h', 5)).isLeft)
    assert(board.move(from = ('d', 5), to = ('g', 5)).isRight)
  }

  test("move diagonal") {
    assert(board.move(from = ('d', 5), to = ('e', 7)).isLeft)
    assert(board.move(from = ('d', 5), to = ('b', 2)).isLeft)
  }
}

