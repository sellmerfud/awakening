import Path.flatRebase

lazy val commonSettings = Seq(
  organization := "org.sellmerfud",
  version      := "1.0",
  scalaVersion := "2.11.0"
)

lazy val stage = taskKey[Unit]("Create distribution zip file")

lazy val awakening = (project in file("."))
  .settings(
    commonSettings,
    name        := "awakening",
    description := "A scala implementation of the solo AI for Labyrinth The Awakening",
    scalacOptions       ++= Seq( "-deprecation", "-unchecked", "-feature" ),
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-pickling" % "0.10.1",
      "org.sellmerfud"         %% "optparse"       % "2.2-SNAPSHOT"
    ),
    // Task to create the distribution zip file
    // To create a zip file that is readable on windoze
    //  1. Remove target/awakening-1.0/.DS_Store, target/awakening-1.0/lib/.DS_Store
    //  2. chmod +x target/awakening-1.0/awakening
    //  3. In the Mac Finder, right click target/awakening-1.0 and compress
    stage in Compile := {
      val log = streams.value.log
      val p = (packageBin in Compile).value  // Depends on the package being built
      val jar    = (artifactPath in packageBin in Compile).value
      val cp     = (managedClasspath in Compile).value.files
      val base   = s"./target/awakening-${version.value}"
      val lib    = s"./target/awakening-${version.value}/lib"
      val others = Seq("src/other/awakening_config",
                       "src/other/awakening",
                       "src/other/awakening.cmd") map (new File(_))
      val files  = (others pair (f => flatRebase(base)(f).map (new File(_)))) ++ 
                   ((jar +: cp) pair (f => flatRebase(lib)(f).map (new File(_))))
      IO.createDirectory(new File(lib))
      IO.copy(files, overwrite = true)
    }
  )
  







