package com.olchovy.chess.domain

import org.scalatest.FunSuite

class QueenSuite extends FunSuite
{
  import Piece.Color._

  val whiteQueen = Queen(white)

  val whitePawn = Pawn(white)

  val blackPawn = Pawn(black)

  val board = ChessBoard(('d', 5) -> whiteQueen, ('g', 2) -> whitePawn, ('b', 5) -> blackPawn)

  test("move up") {
    assert(board.move(from = ('d', 5), to = ('d', 8)).isRight)
  }

  test("move down") {
    assert(board.move(from = ('d', 5), to = ('d', 2)).isRight)
  }

  test("move left") {
    assert(board.move(from = ('d', 5), to = ('a', 5)).isLeft)
    assert(board.move(from = ('d', 5), to = ('b', 5)).isRight)
  }

  test("move right") {
    assert(board.move(from = ('d', 5), to = ('f', 5)).isRight)
  }

  test("move up and left") {
    assert(board.move(from = ('d', 5), to = ('c', 6)).isRight)
    assert(board.move(from = ('d', 5), to = ('a', 8)).isRight)
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
