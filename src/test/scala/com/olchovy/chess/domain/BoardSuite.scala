package com.olchovy.chess.domain

import org.scalatest.FunSuite

class BoardSuite extends FunSuite
{
  val board = new Board(5, 5, Seq(
    "a5", "b5", "c5", "d5", "e5",
    "a4", "b4", "c4", "d4", "e4",
    "a3", "b3", "c3", "d3", "e3",
    "a2", "b2", "c2", "d2", "e2",
    "a1", "b1", "c1", "d1", "e1"
  ).map(Option(_)))

  test("initialize board") {
    intercept[AssertionError] {
      new Board(1, 2, Seq.fill[Option[Int]](5)(None))
    }
  }

  test("address access") {
    assert(Some("a5") == board('a', 5))
    assert(Some("e1") == board('e', 1))
    assert(Some("c3") == board('c', 3))
  }

  test("no slope") {
    val a = board.point(2, 'c')
    val b = board.point(2, 'a')
    assert(!(board.slope(a, b)).isDefined)
  }

  test("0 slope") {
    val a = board.point('b', 4)
    val b = board.point('e', 4)
    assert(board.slope(a, b) == Some(0))
  }

  test("-1 slope (after translation)") {
    val a = board.point('c', 2)
    val b = board.point('e', 4)
    assert(board.slope(a, b) == Some(-1))
  }

  test("1 slope (after translation)") {
    val a = board.point('a', 5)
    val b = board.point('e', 1)
    assert(board.slope(a, b) == Some(1))
  }

  test("upward path") {
    val a = board.point('c', 1)
    val b = board.point('c', 5)

    assert(a.x == 2 && a.y == 4)
    assert(b.x == 2 && b.y == 0)

    val path = board.path(a, b)

    assert(path == Seq(
      Board.Point(2, 3),
      Board.Point(2, 2),
      Board.Point(2, 1),
      Board.Point(2, 0)
    ))
  }

  test("downward path") {
    val a = board.point('b', 4)
    val b = board.point('b', 2)

    assert(a.x == 1 && a.y == 1)
    assert(b.x == 1 && b.y == 3)

    val path = board.path(a, b)

    assert(path == Seq(
      Board.Point(1, 2),
      Board.Point(1, 3)
    ))
  }

  test("leftward path") {
    val a = board.point('e', 3)
    val b = board.point('c', 3)

    assert(a.x == 4 && a.y == 2)
    assert(b.x == 2 && a.y == 2)

    val path = board.path(a, b)

    assert(path == Seq(
      Board.Point(3, 2),
      Board.Point(2, 2)
    ))
  }

  test("rightward path") {
    val a = board.point('c', 3)
    val b = board.point('e', 3)

    assert(a.x == 2 && a.y == 2)
    assert(b.x == 4 && a.y == 2)

    val path = board.path(a, b)

    assert(path == Seq(
      Board.Point(3, 2),
      Board.Point(4, 2)
    ))
  }

  test("invalid path") {
    val a = board.point('a', 1)
    val b = board.point('b', 4)

    assert(a.x == 0 && a.y == 4)
    assert(b.x == 1 && b.y == 1)

    val path = board.path(a, b)

    assert(path == Seq.empty[Board.Point])
  }
}

