package com.olchovy.chess.domain

import org.scalatest.FunSuite

class BishopSuite extends FunSuite
{
  import Piece.Color._

  val piece = Bishop(white)

  val whiteObstacle = Pawn(white)

  val blackObstacle = Pawn(black)

  // B @ d5, P @ g2, p @ b7
  val board = new ChessBoard(
    Seq.fill(9)(None) ++
    Seq(Some(blackObstacle)) ++
    Seq.fill(17)(None) ++
    Seq(Some(piece)) ++
    Seq.fill(26)(None) ++
    Seq(Some(whiteObstacle)) ++
    Seq.fill(9)(None)
  )

  test("move up") {
    assert(board.move(from = ('d', 5), to = ('d', 8)).isLeft)
  }

  test("move down") {
    assert(board.move(from = ('d', 5), to = ('d', 2)).isLeft)
  }

  test("move left") {
    assert(board.move(from = ('d', 5), to = ('a', 5)).isLeft)
  }

  test("move right") {
    assert(board.move(from = ('d', 5), to = ('f', 5)).isLeft)
  }

  test("move up and left") {
    assert(board.move(from = ('d', 5), to = ('c', 6)).isRight)
    assert(board.move(from = ('d', 5), to = ('a', 8)).isLeft)
    assert(board.move(from = ('d', 5), to = ('b', 7)).isRight)
  }

  test("move up and right") {
    assert(board.move(from = ('d', 5), to = ('f', 7)).isRight)
    assert(board.move(from = ('d', 5), to = ('f', 6)).isLeft)
  }

  test("move down and right") {
    assert(board.move(from = ('d', 5), to = ('e', 4)).isRight)
    assert(board.move(from = ('d', 5), to = ('g', 2)).isLeft)
  }

  test("move down and left") {
    assert(board.move(from = ('d', 5), to = ('b', 3)).isRight)
    assert(board.move(from = ('d', 5), to = ('b', 2)).isLeft)
  }
}
