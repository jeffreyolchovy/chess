package com.olchovy.chess.domain

import org.scalatest.FunSuite

class ChessBoardSuite extends FunSuite
{
  import Piece.Color._

  test("build from mapping") {
    val whitePawn = Pawn(white)
    val blackPawn = Pawn(black)
    val board = ChessBoard(('d', 5) -> whitePawn, ('g', 7) -> blackPawn)

    assert(board('d', 5) == Some(whitePawn))
    assert(board('g', 7) == Some(blackPawn))
  }

  test("white's turn") {
    val board = new ChessBoard(Seq.fill(64)(None), turn = 0)
    assert(board.player == white)
  }

  test("black's turn") {
    val board = new ChessBoard(Seq.fill(64)(None), turn = 1)
    assert(board.player == black)
  }

  test("find pieces") {
    val rook = Rook(white)
    val pawn = Pawn(white)
    val board = ChessBoard(('d', 5) -> rook, ('h', 3) -> pawn)

    assert(board.pieces(white) == board.playerPieces)
    assert(board('d', 5) == Some(rook))
    assert(board.playerPieces(board.point('d', 5)) == rook)
    assert(board.playerPieces(board.point('h', 3)) == pawn)
  }

  test("is in check?") {
    val whiteKing = King(white)
    val blackRook = Rook(black)
    val blackBishop = Bishop(black)
    val board = ChessBoard(('b', 7) -> blackBishop, ('d', 5) -> whiteKing, ('g', 5) -> blackRook)

    assert(board.positionOfKing == Some(board.point('d', 5)))
    assert(!(board.threatsToKing.isEmpty))
    assert(board.isInCheck)

    val evasion = board.move(from = ('d', 5), to = ('d', 4))

    assert(evasion.isRight)
    assert(evasion.right.exists(!_.isInCheck))

    val mistake = board.move(from = ('d', 5), to = ('c', 5))

    assert(mistake.isLeft)
  }

  test("two kings in initial positions") {
    val board = ChessBoard(('e', 8) -> King(black), ('e', 1) -> King(white))
    assert(!board.isInCheck)
  }

  /* the remaining tests emply the following flow of movement:
   *
   * 1. create board where black is "losing"
   * 2. move white into position to test for check and checkmate
   * 3. test is black satisfies test's criteria
   */

  test("first checkmate with king and queen") {
    val board = ChessBoard(('d', 8) -> King(black), ('d', 5) -> Queen(white), ('e', 6) -> King(white))
    val result = board.move(from = ('d', 5), to = ('d', 7)) 

    assert(result.isRight)
    
    val Right(nextBoard) = result

    assert(nextBoard.isInCheck)
    assert(nextBoard.isInCheckmate)
  }

  test("second checkmate with king and queen") {
    val board = ChessBoard(('d', 8) -> King(black), ('a', 1) -> Queen(white), ('d', 6) -> King(white))
    val result = board.move(from = ('a', 1), to = ('a', 8)) 

    assert(result.isRight)
    
    val Right(nextBoard) = result

    assert(nextBoard.isInCheck)
    assert(nextBoard.isInCheckmate)
  }

  test("checkmate with king, bishop and knight") {
    val board = ChessBoard(
      ('a', 8) -> King(black),
      ('a', 6) -> Knight(white),
      ('b', 6) -> King(white),
      ('d', 3) -> Bishop(white)
    )

    val result = board.move(from = ('d', 3), to = ('e', 4))

    assert(result.isRight)

    val Right(nextBoard) = result

    assert(nextBoard.isInCheck)
    assert(nextBoard.isInCheckmate)
  }

  test("impossible forced checkmate with king and two knights") {
    val board = ChessBoard(
      ('b', 8) -> King(black),
      ('a', 6) -> Knight(white),
      ('c', 6) -> King(white),
      ('g', 5) -> Knight(white)
    )

    val result = board.move(from = ('c', 6), to = ('b', 6))

    assert(result.isRight)

    val Right(nextBoard) = result

    assert(nextBoard.isInCheck)
    assert(!nextBoard.isInCheckmate)
  }
}

