import java.io.IOException
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, Files, Path, Paths, SimpleFileVisitor}

import cats.Id
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class lab8Test extends AnyFlatSpec with Matchers {
  implicit val fs: RealFileSystem[Id] = new RealFileSystem[Id]
  implicit val printer: ConsolePathPrinter[Id] = new ConsolePathPrinter[Id]
  val program = new Program[Id, Path, Path]

  val tmp: Path = Paths.get("./tmp")
  Files.createDirectory(tmp)

  program.run(tmp)
  val testDir: Path = tmp.resolve("test_dir")
  Files.exists(testDir) shouldBe true
  val dirB: Path = testDir.resolve("b")
  val dirF: Path = testDir.resolve("f")
  Files.exists(dirB) && Files.exists(dirB.resolve("bar")) shouldBe true
  Files.exists(dirB) && Files.exists(dirB.resolve("baz")) shouldBe true
  Files.exists(dirF) && Files.exists(dirF.resolve("foo")) shouldBe true

  Files.walkFileTree(tmp, new SimpleFileVisitor[Path] {
    override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
      Files.delete(file)
      FileVisitResult.CONTINUE
    }
    override def postVisitDirectory(dir: Path, exc: IOException): FileVisitResult = {
      Files.delete(dir)
      FileVisitResult.CONTINUE
    }
  })
}
