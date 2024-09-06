package tofu.concurrent

import cats.data.ReaderT
import cats.effect.concurrent.Deferred
import cats.effect.syntax.concurrent.*
import cats.effect.{Concurrent, ContextShift, IO, Sync}
import cats.syntax.applicativeError.*
import org.scalatest.funsuite.AnyFunSuite
import tofu.compat.unused
import tofu.syntax.monadic.*

import scala.concurrent.ExecutionContext

class PermitSuite extends AnyFunSuite {

  private implicit val ioCS: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

  @unused
  private def summonInstance[I[_]: Sync, F[_]: Concurrent]: MakePermit[I, F] =
    implicitly[MakePermit[I, F]]

  test("check IO has Permit") {
    assert(
      MakePermit[IO, ReaderT[IO, Unit, _]]
        .of(2)
        .flatMap(permitProg(_).run(()))
        .unsafeRunSync() === Left(())
    )
  }

  private def permitProg[F[_]: Concurrent](permit: Permit[F]): F[Either[Unit, Unit]] =
    for {
      wait1       <- Deferred[F, Unit]
      wait2       <- Deferred[F, Unit]
      waitEnd     <- Deferred[F, Unit]
      fiber1      <- permit.withPermit(wait1.complete(()) >> waitEnd.get).start
      fiber2      <- permit.withPermit(wait2.complete(()) >> waitEnd.get).start
      _           <- wait1.get
      _           <- wait2.get
      // fiber3 will be blocked, and throw an IllegalStateException when fiber1 or fiber2 will complete
      fiber3      <- permit.withPermit(waitEnd.complete(()).attempt).start
      resultFiber <- (fiber1.join >> fiber2.join >> fiber3.join).start
      _           <- waitEnd.complete(())
      result      <- resultFiber.join
    } yield result.left.map(_ => ())

}
