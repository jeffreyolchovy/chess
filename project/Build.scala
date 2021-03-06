import sbt._
import Keys._

object Build extends sbt.Build
{
  import Dependencies._

  lazy val myProject = Project("chess", file(".")).settings(
    organization  := "com.olchovy",
    version       := "0.1.0",
    scalaVersion  := "2.9.1",
    scalacOptions := Seq("-deprecation", "-encoding", "utf8"),

    resolvers           ++= Dependencies.repos,
    libraryDependencies ++= compile(jline) ++ test(scalatest) ++ runtime(jline)
  )
}

