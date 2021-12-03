package tofu.concurrent

import cats.Monad
import cats.data.ReaderT
import cats.effect.{Concurrent, IO, Sync}
import org.scalatest.funsuite.AnyFunSuite
import tofu.syntax.monadic._

import scala.annotation.nowarn
import scala.concurrent.ExecutionContext

@nowarn
class QVarSuite extends AnyFunSuite {
  def summonInstance[I[_]: Sync, F[_]: Concurrent] = {
    implicitly[MakeQVar[I, F]]
  }

  implicit val iocs = IO.contextShift(ExecutionContext.global)

  test("check IO has QVar") {
    assert(MakeQVar[IO, ReaderT[IO, Unit, *]].of(1).flatMap(qvarProg(_).run(())).unsafeRunSync() === (1 -> 2))
  }

  def qvarProg[F[_]: Monad](q: QVar[F, Int]) = for {
    x <- q.take
    _ <- q.put(x + 1)
    y <- q.read
  } yield (x, y)
}
