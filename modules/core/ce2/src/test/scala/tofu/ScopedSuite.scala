package tofu

import cats.effect.ContextShift
import cats.effect.Async
import scala.concurrent.ExecutionContext
import cats.effect.Blocker
import tofu.syntax.scoped.*
import tofu.syntax.monadic.*
import scala.concurrent.Future
import tofu.compat.unused

class ScopedSuite {
  @unused
  def doSomething[F[_]: ContextShift: Async, A](fa: F[A], ea: => A)(calcEc: ExecutionContext)(implicit
      ec: ExecutionContext,
      block: Blocker
  ): F[List[A]] = {
    implicit val exec: CalcExec[F] = Scoped.makeExecuteCE2(calcEc)

    for {
      // test for Blocks[F] derivation
      a1 <- blocking(fa)

      // test for BlockingExec[F] derivation, outer ec is shadowed
      a2 <- deferBlockingFuture[F](implicit ec => Future(ea))

      // test for Execute[F] derivation, outer ec is shadowed
      a3 <- deferFuture[F](implicit ec => Future(ea))

      // test for Calculation[F]
      a4 <- calculation(fa)

      // test for CalcExec[F], outer ec is shadowed
      _ <- deferCalcFuture[F](implicit ec => Future(ea))
    } yield List(a1, a2, a3, a4)
  }
}
