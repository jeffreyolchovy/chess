package com.olchovy.chess.cli

/* Behavior to print an sbt-like status message to the console */
sealed trait Message
{
  val level: Message.Level

  val text: String

  val template = "[%s] %s"

  def prettyPrint = println(this)

  override def toString = template.format(level, text)
}

case class ErrorMessage(text: String) extends Message
{
  val level = Message.Level("error", Console.RED)
}

case class NoticeMessage(text: String) extends Message
{
  val level = Message.Level("notice", Console.YELLOW)
}

case class SuccessMessage(text: String) extends Message
{
  val level = Message.Level("success", Console.GREEN)
}

object Message
{
  case class Level(label: String, escapeCode: String)
  {
    override def toString = "%s%s%s".format(escapeCode, label, Console.RESET)
  }
}
