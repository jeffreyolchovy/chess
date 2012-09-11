import sbt._

object Dependencies
{
  val repos = Seq("Typesafe repo [releases]" at "http://repo.typesafe.com/typesafe/releases/")

  def compile   (deps: ModuleID*): Seq[ModuleID] = deps map (_ % "compile")
  def test      (deps: ModuleID*): Seq[ModuleID] = deps map (_ % "test")
  def runtime   (deps: ModuleID*): Seq[ModuleID] = deps map (_ % "runtime")

  val jline     = "org.scala-lang" %  "jline"     % "2.9.1"
  val scalatest = "org.scalatest"  %% "scalatest" % "1.6.1"
}
