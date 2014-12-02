import sbt._
import Keys._

object CillyBuild extends Build {
  lazy val root =
    project
      .in(file("."))
      .settings(
        name := "cilly",
        version := "0.1",
        scalaVersion := "2.11.4",
        scalacOptions ++= Seq("-feature", "-deprecation", "-Xlint","-Xfuture")
      )
}