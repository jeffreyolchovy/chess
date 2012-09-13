package com.olchovy.chess.domain

import org.scalatest.FunSuite

class PawnSuite extends FunSuite
{
  import Piece.Color._

  val whitePawn = Pawn(white)

  val blackPawn = Pawn(black)

  val whiteRook = Rook(white)

  val blackKnight = Knight(black)

  val board = ChessBoard(
    ('d', 7) -> blackPawn,
    ('e', 2) -> whitePawn,
    ('e', 6) -> whiteRook,
    ('d', 3) -> blackKnight
  )

  test("move pawns forward (1)") {
    board.move(from = ('e', 2), to = ('e', 3)) match {
      case Left(e) => throw e
      case Right(newBoard) => assert(newBoard.move(from = ('d', 7), to = ('d', 6)).isRight)
    }
  }

  test("move pawns forward (2)") {
    board.move(from = ('e', 2), to = ('e', 4)) match {
      case Left(e) => throw e
      case Right(newBoard) => assert(newBoard.move(from = ('d', 7), to = ('d', 5)).isRight)
    }
  }

  test("move pawn backward") {
    assert(board.move(from = ('e', 2), to = ('e', 1)).isLeft)
  }

  test("move pawn horizontally") {
    assert(board.move(from = ('e', 2), to = ('f', 2)).isLeft)
  }
}
