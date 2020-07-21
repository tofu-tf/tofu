import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.applicative._
import cats.{Applicative, Defer, Monad, MonoidK, SemigroupK}
import example2.ContinualPrinter
import fs2._
import tofu.common.Console
import tofu.streams.Evals
import tofu.streams.syntax.combineK._
import tofu.streams.syntax.evals._
import tofu.syntax.console._

object PrinterApp extends IOApp with Fs2Instances {
  override def run(args: List[String]): IO[ExitCode] = {
    val printer = new ContinualPrinter[fs2.Stream[IO, *], IO]
    printer.print("hello").compile.drain as ExitCode.Success
  }
}

object example2 {

  trait Printer[F[_]] {
    def print(word: String): F[Unit]
  }

  final class ContinualPrinter[
      S[_]: SemigroupK: Defer: Applicative: Evals[*[_], F],
      F[_]: Console
  ] extends Printer[S] {
    def print(word: String): S[Unit] = word.pure.repeat.evalMap(putStrLn[F])
  }
}

trait Fs2Instances {

  implicit def fs2Evals[F[_]]: Evals[fs2.Stream[F, *], F] =
    new Evals[fs2.Stream[F, *], F] {
      override val monad: Monad[fs2.Stream[F, *]]      = fs2.Stream.monadInstance
      override val monoidK: MonoidK[Stream[F, *]]      = fs2.Stream.monoidKInstance
      override def eval[A](ga: F[A]): fs2.Stream[F, A] = fs2.Stream.eval(ga)
    }
}
