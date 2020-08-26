import cats.Functor
import cats.effect.Sync
import cats.instances.list._
import tofu.common.Console
import tofu.streams._
import tofu.streams.syntax.chunks._
import tofu.streams.syntax.compile._
import tofu.streams.syntax.evals._
import tofu.syntax.monadic._

object example {

  trait Srv[S[_]] {
    def requestAll: S[String]
  }

  object Srv {

    def make[F[_], S[_]: Evals[*[_], F]: Chunks[*[_], C], C[_]: Functor]: Srv[S] = new Impl[F, S, C]

    final class Impl[F[_], S[_]: Chunks[*[_], C]: Evals[*[_], F], C[_]: Functor] extends Srv[S] {
      override def requestAll: S[String]          = evals(getIds).mapChunks(_.map(_ % 2)).evalMap(requestById)
      private def getIds: F[List[Int]]            = ???
      private def requestById(id: Int): F[String] = ???
    }
  }

  final class App[
      F[_]: Console: Sync,
      S[_]: Compile[*[_], F],
      C[_]: Functor
  ](srv: Srv[S]) {
    def run: F[Unit] =
      srv.requestAll.to[Vector] >>= (xs => Console[F].putStr(xs.mkString(", ")))
  }
}
