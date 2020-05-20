import cats.effect.{ExitCode, IO, IOApp, Resource}

import scala.concurrent.duration._
import cats.syntax.all._
import cats.effect.concurrent.{MVar, Ref, Semaphore}



object App extends IOApp {

  def runPrinter(mvar: MVar[IO, String]): Resource[IO, Unit] = {
    def rec: IO[Unit] = for {
      value <- mvar.take
      _ <- IO(println(value))
      _ <- rec
    } yield ()
    Resource.make(rec.start)(_.cancel.flatMap(_ => IO(println("Close printer")))).void
  }

  def runCounter(mvar: MVar[IO, String]): Resource[IO, Unit] = {
    def rec(cnt: Long): IO[Unit] = for {
      _ <- IO.sleep(1.seconds)
      _ <- mvar.put((cnt + 1).toString)
      _ <- rec(cnt + 1)
    } yield ()
    Resource.make(rec(0).start)(_.cancel.flatMap(_ => IO(println("Close counter")))).void
  }

  val program: Resource[IO, Unit] = for {
    mvar <- Resource.make(MVar.empty[IO, String])(_ => IO(print("Interrupted")))
    _ <- runCounter(mvar)
    _ <- runPrinter(mvar)
  } yield ()


  override def run(args: List[String]): IO[ExitCode] =
    program.use(_ => IO.never)

}
