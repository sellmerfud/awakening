import Path.FileMap
import java.nio.file.{ Files, Paths }
import java.nio.file.attribute.PosixFilePermissions
import scala.sys.process._

lazy val commonSettings = Seq(
  organization := "org.sellmerfud",
  version      := "6.5",
  scalaVersion := "2.13.18",
  javacOptions        ++= Seq("-source", "8", "-target",  "8"),
  scalacOptions       ++= Seq( "-deprecation", "-unchecked", "-feature" ),
)

lazy val stage       = taskKey[Unit]("Create distribution zip file")
lazy val sourceOther = settingKey[File]("Other source file included in the package")

lazy val awakening = (project in file("."))
  .settings(
    commonSettings,
    name        := "awakening",
    description := "A scala implementation of the solo AI for Labyrinth The Awakening",
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1",
      "org.sellmerfud"         %% "optparse"       % "2.2",
      "org.scalactic"         %% "scalactic"       % "3.0.8"
    ),
    sourceOther := sourceDirectory.value / "other",
    Compile / resourceGenerators += Def.task {
      val versFile = (Compile / resourceManaged).value / "version"
      IO.write(versFile, version.value)
      Seq(versFile)
      }.taskValue,
    // Task to create the distribution zip file
    Compile / stage := {
      val log = streams.value.log
      (loader / Compile / packageBin).value  // Depends on the loader package being built
      (Compile / packageBin).value           // Depends on the main package being built
      def rebaseTo(directory: File)(origfile: File): Option[File] = {
        val mapper: FileMap = Path.flat(directory)
        mapper(origfile)
      }
      val pkgDir     = target.value / s"awakening-${version.value}"
      val lib        = pkgDir / "lib"
      val doc        = pkgDir / "doc"
      val loader_jar = (loader / Compile / packageBin / artifactPath).value
      val zipfile    = file(s"${pkgDir.getAbsolutePath}.zip")
      val jars       = (Compile / fullClasspathAsJars).value.files
      val others     = (sourceOther.value * "*").get
      val assets     = (others pair rebaseTo(pkgDir)) ++ (jars pair rebaseTo(lib))
      
      log.info(s"Staging to $pkgDir ...")
      IO.delete(pkgDir)
      IO.createDirectory(lib)
      IO.createDirectory(doc)
      IO.copyFile(loader_jar, lib / loader_jar.getName)
      IO.copy(assets, CopyOptions().withOverwrite(true))
      s"typst compile src/typst/enh-bot-instructions.typ ${doc}/enh-bot-instructions.pdf" ! log
      s"typst compile src/typst/enhanced-evo-diagram.typ ${doc}/enhanced-evo-diagram.pdf" ! log
      IO.setPermissions(pkgDir / "awakening", "rwxr-xr-x") // Make bash script executable
      // Create zip file
      (pkgDir ** ".DS_Store").get foreach IO.delete
      val zipEntries = (pkgDir ** "*").get map (f => (f, IO.relativize(target.value, f).get) )
      IO.zip(zipEntries, zipfile, None)
    }
  )
  
  lazy val loader = (project in file("loader"))
    .settings(
      commonSettings,
      name        := "Loader",
      description := "Bootstrap loader",
      // Make loader.jar generic without version number so the fitl scripts can find it.
      (Compile / packageBin / artifactPath) := (Compile / target).value / "loader.jar"
    )
  