package com.olchovy.chess.cli

import tools.jline.console.ConsoleReader
import util.parsing.combinator.RegexParsers

/* CLI two-player chess application
 *
 * @see main()
 */
object Application
{
  import com.olchovy.chess.domain._

  val welcome = "Beginning a game of two-player chess. Type 'help' for more information."

  val prompt = "> "

  val inputReader = new ConsoleReader

  val inputParser = CommandDSL

  @annotation.tailrec
  def run(board: ChessBoard): Unit = {
    // print the board to the console
    board.prettyPrint

    // test for endgame
    // the quickest possible checkmate can be achieved in two "moves" (where two of our turns makes one move)
    if(board.turn >= 4 && board.isInCheckmate) {
      SuccessMessage("Checkmate. %s wins.".format(board.opponentName.capitalize)).prettyPrint
      Quit.execute(board)
    }

    if(board.isInCheck) { 
      NoticeMessage("%s is in check.".format(board.playerName.capitalize)).prettyPrint
    }

    // prompt player for input
    NoticeMessage("%s's move.".format(board.playerName.capitalize)).prettyPrint

    // parse command line input with command dsl 
    inputParser(inputReader.readLine(prompt)) match {
      // parse error
      case Left(e) => 
        ErrorMessage(e.getMessage).prettyPrint
        run(board)

      // parse success, execute command
      case Right(command) => run(command.execute(board))
    }
  }

  sealed trait Command
  {
    def execute(board: ChessBoard): ChessBoard
  }

  /* Mixin for commands that are more or less side-effects.
   *
   * The original board passed to the execute method will be returned upon completion.
   */
  trait SideEffect
  {
    this: Command =>

    def sideEffect(board: ChessBoard): Unit

    def execute(board: ChessBoard) = { sideEffect(board); board }
  }

  case class Move(from: (Char, Int), to: (Char, Int)) extends Command
  {
    def execute(board: ChessBoard) = board.move(from, to) match {
      case Left(e) =>
        ErrorMessage(e.getMessage).prettyPrint
        board

      case Right(newBoard) => newBoard
    }
  }

  object Help extends Command with SideEffect
  {
    def sideEffect(board: ChessBoard) = println(
      """Play chess by typing a command at the prompt.

      |Navigate the board by using the alphanumeric coordinates.
      |White pieces are located at the bottom of the board, black are up top.
      |White pieces are capitalized.
      |White goes first.
      
      |Available commands are:
      
      |  h, help               - display this message
      |  m, move <from> <to>   - move a piece from <from> to <to>, e.g. m d2 d4
      |  p, print              - redraw the board to the console
      |  q, quit               - exit the program
      |
      |NOTE: Since players will primarily be moving pieces, typing 'm' or 'move' before piece coordinates is optional.
      """.stripMargin.trim
    )
  }

  object Print extends Command with SideEffect
  {
    def sideEffect(board: ChessBoard) = { /* no-op */ }
  }

  object Quit extends Command with SideEffect
  {
    def sideEffect(board: ChessBoard) = sys.exit(0)
  }
  
  object CommandDSL extends RegexParsers
  {
    private lazy val EOL = """\z""".r

    private lazy val char: Parser[Char] = "[a-zA-Z]{1}".r ^^ {
      case string => string.toLowerCase.apply(0)
    }

    private lazy val int: Parser[Int] = "[0-9]".r ^^ {
      case string => string.toInt
    }

    protected[cli] lazy val position: Parser[(Char, Int)] = char ~ int ^^ {
      case a ~ b => (a, b)
    }

    protected[cli] lazy val move: Parser[Command] = ("move|m".r ?) ~ position ~ position ^^ {
      case _ ~ from ~ to => Move(from, to)
    }

    protected[cli] lazy val help: Parser[Command] = "help|h".r ^^ {
      case _ => Help
    } 

    protected[cli] lazy val print: Parser[Command] = "print|p".r ^^ {
      case _ => Print
    }

    protected[cli] lazy val quit: Parser[Command] = "quit|q".r ^^ { 
      case _ => Quit
    }

    protected[cli] lazy val command: Parser[Command] = ( move | help | print | quit ) ~ EOL ^^ {
      case command ~ _ => command
    }

    def apply(input: String): Either[Throwable, Command] = try {
      parse(command, input) match {
        case (e: NoSuccess) => Left(new RuntimeException("Syntax error. Type 'help' for usage."))
        case Success(command, _) => Right(command)
      }
    } catch {
      case (e: NullPointerException) => Left(new RuntimeException("Enter a command. Type 'help' for usage."))
      case e =>
        ErrorMessage("Program encountered an unexpected error: %s. Terminating.".format(e.getMessage)).prettyPrint
        Right(Quit)
    }
  }

  def main(args: Array[String]) {
    println(welcome)
    run(ChessBoard())
  }
}
