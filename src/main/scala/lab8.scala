import java.nio.file.{Files, Path, Paths}

import cats.data.{IndexedStateT, StateT}
import cats.{Applicative, Functor, Id, Monad}
import cats.syntax.all._
import cats.instances.list._

import scala.collection.JavaConverters._

import scala.language.higherKinds

trait MkDir[F[_], Dir] {
  def mkDir(dir: Dir, name: String): F[Dir]
}

trait MkFile[F[_], Dir, File] {
  def mkFile(dir: Dir, name: String): F[File]
}

trait GetDirFiles[F[_], Dir, File] {
  def getDirFiles(dir: Dir): F[List[File]]
}

trait GetFileName[F[_], File] {
  def getFileName(file: File): F[String]
}

trait MoveFile[F[_], Dir, File] {
  def moveFile(dir: Dir, file: File): F[File]
}

trait Printer[F[_], File] {
  def printName(file: File): F[Unit]
}

class Program[F[_], Dir, File](implicit
                               F: Monad[F],
                               mkDir: MkDir[F, Dir],
                               mkFile: MkFile[F, Dir, File],
                               getDirFiles: GetDirFiles[F, Dir, File],
                               getFileName: GetFileName[F, File],
                               moveFile: MoveFile[F, Dir, File],
                               printer: Printer[F, File]) {
  def run(dir: Dir): F[Unit] = for {
    testDir <- mkDir.mkDir(dir, "test_dir")
    file1 <- mkFile.mkFile(testDir, "foo")
    file2 <- mkFile.mkFile(testDir, "bar")
    file3 <- mkFile.mkFile(testDir, "baz")
    files <- getDirFiles.getDirFiles(testDir)
    _ <- files.traverse(file => printer.printName(file))
    namesStarts <- files.traverse(file => getFileName.getFileName(file).map(_.head))
    dirs <- namesStarts.traverse(c => mkDir.mkDir(testDir, c.toString))
    _ <- files.zip(dirs).traverse(p => moveFile.moveFile(p._2, p._1))
  } yield ()
}

class RealFileSystem[F[_] : Applicative] extends MkDir[F, Path] with MkFile[F, Path, Path]
  with GetDirFiles [F, Path, Path] with GetFileName[F, Path] with MoveFile[F, Path, Path] {
  override def mkDir(dir: Path, name: String): F[Path] =
    if (!Files.exists(dir.resolve(name)))
      Files.createDirectories(dir.resolve(name)).pure[F]
  else
      dir.resolve(name).pure[F]

  override def mkFile(dir: Path, name: String): F[Path] =
    if (!Files.exists(dir.resolve(name)))
      Files.createFile(dir.resolve(name)).pure[F]
    else
      dir.resolve(name).pure[F]

  override def getDirFiles(dir: Path): F[List[Path]] =
    Files.list(dir).filter(Files.isRegularFile(_)).iterator().asScala.toList.pure[F]

  override def getFileName(file: Path): F[String] =
    file.getFileName.toString.pure[F]

  override def moveFile(dir: Path, file: Path): F[Path] =
    if (!Files.exists(dir.resolve(file.getFileName)))
      Files.move(file, dir.resolve(file.getFileName)).pure[F]
    else
      dir.resolve(file.getFileName).pure[F]
}

class ConsolePathPrinter[F[_] : Applicative] extends Printer[F, Path] {
  override def printName(file: Path): F[Unit] = println(file.getFileName).pure[F]
}

case class FakeDir(name: String, dirs: List[FakeDir], files: List[FakeFile])

case class FakeFile(name: String)

case class FileRef(path: List[String])

trait ModifyFakeDir[F[_]] {
  def modifyDir(fileRef: FileRef)(f: FakeDir => FakeDir): F[Unit]
}

object ModifyFakeDir {
  def apply[F[_]](implicit m: ModifyFakeDir[F]): ModifyFakeDir[F] = m
}

class FakeFileSystem[F[_] : ModifyFakeDir : Functor] extends MkDir[F, FileRef] with MkFile[F, FileRef, FileRef] {
  override def mkDir(dir: FileRef, name: String): F[FileRef] = ModifyFakeDir[F].modifyDir(dir) { currentDir =>
    currentDir.copy(dirs = FakeDir(name, Nil, Nil) :: currentDir.dirs)
  }.map(_ => FileRef(dir.path :+ name))

  override def mkFile(dir: FileRef, name: String): F[FileRef] = ModifyFakeDir[F].modifyDir(dir) { currentDir =>
    currentDir.copy(files = FakeFile(name) :: currentDir.files)
  }.map(_ => FileRef(dir.path :+ name))
}

class StateTModify[F[_] : Applicative] extends ModifyFakeDir[StateT[F, FakeDir, *]] {
  private def modifyDir(path: List[String], dir: FakeDir, f: FakeDir => FakeDir): FakeDir = path match {
    case Nil => f(dir)
    case head :: tail => dir.copy(dirs = dir.dirs.map {
      case current if current.name == head => modifyDir(tail, current, f)
      case current => current
    })
  }

  override def modifyDir(fileRef: FileRef)(f: FakeDir => FakeDir): StateT[F, FakeDir, Unit] = StateT.modify[F, FakeDir] { root =>
    modifyDir(fileRef.path, root, f)
  }
}

class ConsoleFakePrinter[F[_] : Applicative] extends Printer[F, FileRef] {
  override def printName(file: FileRef): F[Unit] = println(file.path.mkString(".")).pure[F]
}


object TypeClasses {
  def main(args: Array[String]): Unit = {
        implicit val fs: RealFileSystem[Id] = new RealFileSystem[Id]
        implicit val printer: ConsolePathPrinter[Id] = new ConsolePathPrinter[Id]
        val program = new Program[Id, Path, Path]

        program.run(Paths.get("."))



//    type MyMonad[A] = StateT[Id, FakeDir, A]
//
//    implicitly[Functor[MyMonad]]
//
//    implicit val modifier: StateTModify[Id] = new StateTModify[Id]
//    implicit val fs: FakeFileSystem[StateT[Id, FakeDir, *]] = new FakeFileSystem[MyMonad]
//    implicit val printer: ConsoleFakePrinter[StateT[Id, FakeDir, *]] = new ConsoleFakePrinter[MyMonad]
//    val program = new Program[MyMonad, FileRef, FileRef]
//
//    println(program.run(FileRef(List.empty)).runS(FakeDir(".", Nil, Nil)))
  }
}