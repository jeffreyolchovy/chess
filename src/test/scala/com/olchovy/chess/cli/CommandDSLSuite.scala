package com.olchovy.chess.cli

import org.scalatest.FunSuite

class CommandDSLSuite extends FunSuite
{
  import Application.CommandDSL

  test("help") {
    assert(CommandDSL("h").isRight)
    assert(CommandDSL("help").isRight)
    assert(CommandDSL("h garbage").isLeft)
  }

  test("move (and syntax sugar)") {
    assert(CommandDSL("m").isLeft)
    assert(CommandDSL("move").isLeft)

    assert(CommandDSL("m b4 g3").isRight)
    assert(CommandDSL("move b4 g3").isRight)
    assert(CommandDSL("move b4 g3 garbage").isLeft)

    assert(CommandDSL("b4 g3").isRight)
    assert(CommandDSL("b4 g3 garbage").isLeft)
    assert(CommandDSL("4b 3g garbage").isLeft)
  }

  test("print") {
    assert(CommandDSL("p").isRight)
    assert(CommandDSL("print").isRight)
    assert(CommandDSL("p garbage").isLeft)
  }

  test("quit") {
    assert(CommandDSL("q").isRight)
    assert(CommandDSL("quit").isRight)
    assert(CommandDSL("q garbage").isLeft)
  }
}
