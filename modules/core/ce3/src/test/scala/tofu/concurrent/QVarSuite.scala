package tofu.concurrent

import cats.Monad
import cats.data.ReaderT
import cats.effect.unsafe.IORuntime
import cats.effect.{Async, IO, Sync}
import org.scalatest.funsuite.AnyFunSuite
import tofu.syntax.monadic._

import scala.annotation.nowarn

@nowarn
class QVarSuite extends AnyFunSuite {
  implicit val iort: IORuntime = IORuntime.global

  def summonInstance[I[_]: Sync, F[_]: Async] = {
    implicitly[MakeQVar[I, F]]
  }

  test("check IO has QVar") {
    assert(MakeQVar[IO, ReaderT[IO, Unit, _]].of(1).flatMap(qvarProg(_).run(())).unsafeRunSync() === (1 -> 2))
  }

  def qvarProg[F[_]: Monad](q: QVar[F, Int]) = for {
    x <- q.take
    _ <- q.put(x + 1)
    y <- q.read
  } yield (x, y)
}
