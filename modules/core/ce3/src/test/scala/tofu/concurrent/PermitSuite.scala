package tofu.concurrent

import cats.data.ReaderT
import cats.effect.kernel.Deferred
import cats.effect.syntax.spawn.*
import cats.effect.unsafe.IORuntime
import cats.effect.{Async, Concurrent, IO, Sync}
import org.scalatest.funsuite.AnyFunSuite
import tofu.compat.unused
import tofu.syntax.monadic.*

class PermitSuite extends AnyFunSuite {
  private implicit val iort: IORuntime = IORuntime.global

  @unused
  private def summonInstance[I[_]: Sync, F[_]: Async]: MakePermit[I, F] =
    implicitly[MakePermit[I, F]]

  test("check Permit") {
    assert(
      MakePermit[IO, ReaderT[IO, Unit, _]]
        .of(2)
        .flatMap(permitProg(_).run(()))
        .unsafeRunSync() === false
    )
  }

  private def permitProg[F[_]: Concurrent](permit: Permit[F]): F[Boolean] =
    for {
      wait1       <- Deferred[F, Unit]
      wait2       <- Deferred[F, Unit]
      waitEnd     <- Deferred[F, Unit]
      fiber1      <- permit.withPermit(wait1.complete(()) >> waitEnd.get).start
      fiber2      <- permit.withPermit(wait2.complete(()) >> waitEnd.get).start
      _           <- wait1.get
      _           <- wait2.get
      // fiber3 will be blocked, and return false (on complete) when fiber1 or fiber2 will complete
      fiber3      <- permit.withPermit(waitEnd.complete(())).start
      resultFiber <- (fiber1.join >> fiber2.join >> fiber3.join).flatMap(_.embedNever).start
      _           <- waitEnd.complete(())
      result      <- resultFiber.join.flatMap(_.embedNever)
    } yield result

}
