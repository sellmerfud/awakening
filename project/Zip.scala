import java.io.File
import java.nio.file.Files
import scala.collection.Traversable
import scala.jdk.CollectionConverters._
import org.apache.commons.compress.archivers.zip.{ZipArchiveOutputStream, ZipArchiveEntry}
import java.nio.file.attribute.PosixFilePermission
import PosixFilePermission._
import org.apache.commons.io.FileUtils

// A zip file creator that saves unix file permissions in the zip file entries
object Zip {
  def createZipFile(entries: Traversable[(File, String)], zipFile: File): Unit = {
    val archive = new ZipArchiveOutputStream(zipFile)
    for ((file, name) <- entries) {
      val entry = archive.createArchiveEntry(file, name)
      entry.setUnixMode(permsToInt(Files.getPosixFilePermissions(file.toPath).asScala.toSet))
      archive.putArchiveEntry(entry)
      if (file.isFile)
        FileUtils.copyFile(file, archive)
      archive.closeArchiveEntry()
    }
    archive.finish();
  }

  private def permsToInt(perms: Set[PosixFilePermission]): Int = perms.foldLeft(0) {
    case (p, OWNER_READ)     => p | 256
    case (p, OWNER_WRITE)    => p | 128
    case (p, OWNER_EXECUTE)  => p | 64
    case (p, GROUP_READ)     => p | 32
    case (p, GROUP_WRITE)    => p | 16
    case (p, GROUP_EXECUTE)  => p | 8
    case (p, OTHERS_READ)    => p | 4
    case (p, OTHERS_WRITE)   => p | 2
    case (p, OTHERS_EXECUTE) => p | 1
  }        
}
