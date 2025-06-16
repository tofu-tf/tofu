package tofu.concurrent

import cats.effect.Sync
import cats.effect.IO
import cats.data.ReaderT

import org.scalatest.funsuite.AnyFunSuite
import cats.effect.Concurrent
import cats.Monad
import tofu.syntax.monadic._
import scala.concurrent.ExecutionContext
import cats.effect.ContextShift

class QVarSuite extends AnyFunSuite {
  def summonInstance[I[_]: Sync, F[_]: Concurrent] = {
    implicitly[MakeQVar[I, F]]
  }

  implicit val iocs: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

  test("check IO has QVar") {
    assert(MakeQVar[IO, ReaderT[IO, Unit, _]].of(1).flatMap(qvarProg(_).run(())).unsafeRunSync() === (1 -> 2))
  }

  def qvarProg[F[_]: Monad](q: QVar[F, Int]) = for {
    x <- q.take
    _ <- q.put(x + 1)
    y <- q.read
  } yield (x, y)
}
