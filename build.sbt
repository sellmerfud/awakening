import Path.flatRebase
import java.nio.file.{ Files, Paths }
import java.nio.file.attribute.PosixFilePermission
import java.nio.file.attribute.PosixFilePermission._
import java.util.HashSet

lazy val commonSettings = Seq(
  organization := "org.sellmerfud",
  version      := "4.21",
  scalaVersion := "2.13.10"
)

lazy val stage = taskKey[Unit]("Create distribution zip file")

lazy val awakening = (project in file("."))
  .settings(
    commonSettings,
    name        := "awakening",
    description := "A scala implementation of the solo AI for Labyrinth The Awakening",
    scalacOptions       ++= Seq( "-deprecation", "-unchecked", "-feature" ),
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1",
      "org.sellmerfud"         %% "optparse"       % "2.2",
      "org.scalactic"         %% "scalactic"       % "3.0.8"
    ),
    // Task to create the distribution zip file
    Compile / stage := {
      val log = streams.value.log
      (Compile / packageBin).value  // Depends on the package being built
      val jar    = (Compile / packageBin/ artifactPath).value
      // Filter out the scala-compiler jar file.
      val cp     = (Compile / managedClasspath).value.files filterNot (_.getName contains "compiler")
      val base   = s"./target/awakening-${version.value}"
      val lib    = s"./target/awakening-${version.value}/lib"
      val others = Seq("src/other/README.txt",
                       "src/other/awakening_config",
                       "src/other/awakening",
                       "src/other/awakening.cmd") map (new File(_))
      val files  = (others pair (f => flatRebase(base)(f).map (new File(_)))) ++ 
                   ((jar +: cp) pair (f => flatRebase(lib)(f).map (new File(_))))
      log.info(s"Staging to $base ...")
      IO.delete(new File(s"$base.zip"))
      IO.delete(new File(base))
      IO.createDirectory(new File(lib))
      IO.copy(files, CopyOptions().withOverwrite(true))
      val perms = new HashSet[PosixFilePermission]()
      val permsList = List(OWNER_READ,  OWNER_WRITE,   OWNER_EXECUTE, 
                           GROUP_READ,  GROUP_EXECUTE,
                           OTHERS_READ, OTHERS_EXECUTE)
      for (p <- permsList) 
        perms.add(p)
      Files.setPosixFilePermissions(Paths.get(s"$base/awakening"), perms)
      log.info("Done staging.")
    }
  )
  







