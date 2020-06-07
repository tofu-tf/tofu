import cats.FlatMap
import cats.effect.ExitCode
import cats.instances.list._
import tofu.common.Console
import tofu.streams._
import tofu.streams.syntax.compile._
import tofu.streams.syntax.evals._
import tofu.syntax.monadic._

object playground {

  trait Srv[S[_]] {
    def requestAll: S[String]
  }

  object Srv {
    def apply[F[_], S[_]: Evals[*[_], F]]: Srv[S] = new Impl[F, S]
    final class Impl[F[_], S[_]](implicit S: Evals[S, F]) extends Srv[S] {
      override def requestAll: S[String]          = S.evals(getIds).evalMap(requestById)
      private def getIds: F[List[Int]]            = ???
      private def requestById(id: Int): F[String] = ???
    }
  }

  final class App[F[_]: Console: FlatMap, S[_]: Evals[*[_], F]: Compile[*[_], F]] {

    def run: F[ExitCode] = {
      val srv = Srv[F, S]
      srv.requestAll.compile >>= (xs => Console[F].putStr(xs.mkString(", ")) as ExitCode.Success)
    }

  }
}
