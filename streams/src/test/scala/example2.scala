import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.applicative._
import cats.{Alternative, Applicative, Apply, Monad}
import example2.ContinualPrinter
import fs2._
import tofu.common.Console
import tofu.streams.syntax.combineK._
import tofu.streams.syntax.evals._
import tofu.streams.{CombineK, Evals}
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

  implicit def fs2Alternative[F[_]]: Alternative[Stream[F, *]] =
    new Alternative[Stream[F, *]] {
      override def ap[A, B](ff: Stream[F, A => B])(fa: Stream[F, A]): Stream[F, B] =
        implicitly[Apply[Stream[F, *]]].ap(ff)(fa)
      override def empty[A]: Stream[F, A]                                          = Stream.empty
      override def combineK[A](x: Stream[F, A], y: Stream[F, A]): Stream[F, A]     = x ++ y
      override def pure[A](x: A): Stream[F, A]                                     = Stream.emit(x)
    }

  implicit def fs2Evals[F[_]]: Evals[fs2.Stream[F, *], F] =
    new Evals[fs2.Stream[F, *], F] {
      override val monad: Monad[fs2.Stream[F, *]]         = fs2.Stream.monadInstance
      override val alternative: Alternative[Stream[F, *]] = implicitly
      override def eval[A](ga: F[A]): fs2.Stream[F, A]    = fs2.Stream.eval(ga)
    }
}
