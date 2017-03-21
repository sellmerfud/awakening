name := "awakening"

description := "A scala implementation of the solo AI for Labyrinth The Awakening"

organization := "org.sellmerfud"

version := "1.0"

scalaVersion := "2.11.0"

scalacOptions ++= Seq( "-deprecation", "-unchecked", "-feature" )

libraryDependencies += "org.scala-lang.modules" %% "scala-pickling" % "0.10.1"
libraryDependencies += "org.sellmerfud" % "optparse_2.11" % "2.2-SNAPSHOT"
