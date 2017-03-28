name := "awakening"

description := "A scala implementation of the solo AI for Labyrinth The Awakening"

organization := "org.sellmerfud"

version := "1.0"

scalaVersion := "2.11.0"

scalacOptions ++= Seq( "-deprecation", "-unchecked", "-feature" )

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-pickling" % "0.10.1",
  "org.sellmerfud"         %% "optparse"       % "2.2-SNAPSHOT"
)
