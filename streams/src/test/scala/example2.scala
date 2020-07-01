import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.applicative._
import cats.syntax.foldable._
import cats.{Applicative, Foldable, Monad}
import example2.ContinualPrinter
import fs2._
import tofu.common.Console
import tofu.streams.syntax.combineK._
import tofu.streams.syntax.evals._
import tofu.streams.{Evals, CombineK}
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
      S[_]: CombineK: Applicative: Evals[*[_], F],
      F[_]: Console
  ] extends Printer[S] {
    def print(word: String): S[Unit] = word.pure.repeat.evalMap(putStrLn[F])
  }
}

trait Fs2Instances {

  implicit def fs2CombineK[F[_]]: CombineK[Stream[F, *]] =
    new CombineK[Stream[F, *]] {
      override def combineK_[A](a: Stream[F, A])(b: => Stream[F, A]): Stream[F, A] = a ++ b
    }

  implicit def fs2Evals[F[_]]: Evals[fs2.Stream[F, *], F] =
    new Evals[fs2.Stream[F, *], F] {
      override val monad: Monad[fs2.Stream[F, *]]                       = fs2.Stream.monadInstance
      override def eval[A](ga: F[A]): fs2.Stream[F, A]                  = fs2.Stream.eval(ga)
      override def emits[C[_]: Foldable, A](as: C[A]): fs2.Stream[F, A] = fs2.Stream.emits(as.toList)
    }
}
