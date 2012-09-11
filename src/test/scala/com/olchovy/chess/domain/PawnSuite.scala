package com.olchovy.chess.domain

import org.scalatest.FunSuite

class PawnSuite extends FunSuite
{
  import Piece.Color._

  val whitePiece = Pawn(white)

  val blackPiece = Pawn(black)

  val whiteObstacle = Rook(white)

  val blackObstacle = Knight(black)

  // p @ d7, P @ e2, R @ e6, n @ d3
  val board = new ChessBoard(
    Seq.fill(8)(None) ++
    Seq.fill(3)(None) ++ Seq(Some(blackPiece)) ++ Seq.fill(4)(None) ++
    Seq.fill(4)(None) ++ Seq(Some(whiteObstacle)) ++ Seq.fill(3)(None) ++
    Seq.fill(16)(None) ++
    Seq.fill(3)(None) ++ Seq(Some(blackObstacle)) ++ Seq.fill(4)(None) ++
    Seq.fill(4)(None) ++ Seq(Some(whitePiece)) ++ Seq.fill(3)(None) ++
    Seq.fill(8)(None)
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

