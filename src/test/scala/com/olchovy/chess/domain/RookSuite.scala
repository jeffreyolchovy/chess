package com.olchovy.chess.domain

import org.scalatest.FunSuite

class RookSuite extends FunSuite
{
  import Piece.Color._

  val whiteRook = Rook(white)

  val whitePawn = Pawn(white)

  val blackPawn = Pawn(black)

  val board = ChessBoard(('d', 5) -> whiteRook, ('b', 5) -> whitePawn, ('g', 5) -> blackPawn)

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
