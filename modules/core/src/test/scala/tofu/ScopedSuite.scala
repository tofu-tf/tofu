package tofu

import scala.annotation.nowarn
import scala.concurrent.{ExecutionContext, Future}

import cats.effect.{Async, Blocker, ContextShift}
import tofu.syntax.monadic._
import tofu.syntax.scoped._

class ScopedSuite {
  @nowarn("msg=parameter value")
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
      a5 <- deferCalcFuture[F](implicit ec => Future(ea))
    } yield List(a1, a2, a3, a4)
  }
}
