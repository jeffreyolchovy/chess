package com.olchovy.chess.domain

/* A cartesian plane addressable by (i, j).
 *
 * `i` represents characters beginning with ASCII value 97.
 * 
 * `j` represents integers beginning with the total number of rows.
 *  
 * For non-empty boards, `i` will always begin with 'a', and `j` will always terminate at 1.
 *
 * e.g. a 3 x 3 board contains the addresses:
 *
 * a3 b3 c3
 * a2 b2 c2
 * a1 b1 c1
 *
 * The board is internally translated into a more traditional coordinate system
 * in order to facilitate operations.
 *
 * The 3 x 3 board above would be translated to:
 *
 * (0, 0) (1, 0) (2, 0)
 * (0, 1) (1, 1) (2, 1) 
 * (0, 2) (1, 2) (2, 2)
 *
 * Items of type `A` may be stored at any of the board's addresses.
 */
class Board[A](val numCols: Int, val numRows: Int, protected val state: Seq[Option[A]])
{
  import Board._

  assert(numCols * numRows == state.size)

  /* access the state at address (col, row) */
  def apply(col: Char, row: Int): Option[A] = apply(point(col, row))

  /* access the state at the address of translated coordinate */
  def apply(p: Point): Option[A] = if(contains(p)) state(indexOf(p)) else None

  def contains(col: Char, row: Int): Boolean = contains(point(col, row))

  def contains(p: Point): Boolean = p.x >= 0 && p.x < numCols && p.y >=0 && p.y < numRows

  protected[domain] def point(col: Char, row: Int): Point = Point(col - 'a', numRows - row)

  /* translate coordinates to int value (for accessing state addresses) */
  protected def indexOf(p: Point): Int = p.x + p.y * numCols

  protected[domain] def slope(a: Point, b: Point): Option[Double] = {
    val deltaY = b.y - a.y
    val deltaX = b.x - a.x

    if(deltaX == 0) None else Some(deltaY.toDouble / deltaX.toDouble)
  }

  /* the sequence of points from (a, b] */
  protected[domain] def path(a: Point, b: Point): Seq[Point] = {
    // if the predicate is not satisified, negate the value of i
    def invert_?(f: => Boolean)(i: Int) = i * (if(f) 1 else -1)
    
    slope(a, b) match {
      // occupy same column
      case None => walk(a, b, rise = invert_?(b.y > a.y)(1), run = 0)

      // occupy same row
      case Some(0d) => walk(a, b, rise = 0, run = invert_?(b.x > a.x)(1))

      // occupy same diagonal
      case Some(n) if n == 1 || n == -1 => walk(a, b, rise = invert_?(b.y > a.y)(1), run = invert_?(b.x > a.x)(1))

      // invalid slope, no straight path exists between a and b
      case _ => Seq.empty[Point]
    }
  }

  /* recursively navigate from a to b using only straight lines */
  private def walk(a: Point, b: Point, rise: Int, run: Int): Seq[Point] = {
    @annotation.tailrec
    def loop(a: Point, acc: List[Point] = Nil): List[Point] = {
      if(a == b) acc else {
        val c = Point(a.x + run, a.y + rise)
        loop(c, c :: acc)
      }
    }

    loop(a).reverse
  }
}

object Board
{
  case class Point(x: Int, y: Int)
}
