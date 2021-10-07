package tofu
package syntax

import cats.FlatMap
import tofu.syntax.monadic._
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

object scopedKernel extends ScopedSyntax

trait ScopedSyntax {
  import Scoped.{Blocking, Calculation, Main}
  import ScopedSyntax._

  private type Blocks[F[_]]     = kernel.types.Blocks[F]     // avoid import for unidoc
  private type Calculates[F[_]] = kernel.types.Calculates[F] // avoid import for unidoc
  private type Execute[F[_]]    = kernel.types.Execute[F]    // avoid import for unidoc

  /** run process in scope */
  def scoped[Tag] = new ScopedApply[Tag]

  /** delay calculation in scope */
  def scopedDelay[Tag, F[_]] = new ScopedDelay[Tag, F]

  def blockingDelay[F[_]] = scopedDelay[Scoped.Blocking, F]

  def calcDelay[F[_]] = scopedDelay[Scoped.Calculation, F]

  /** run process in blocking scope */
  def blocking[F[_]: Blocks, A](fa: F[A]): F[A] = scoped[Blocking](fa)

  /** run process in calculation scope */
  def calculation[F[_]: Calculates, A](fa: F[A]): F[A] = scoped[Calculation](fa)

  /** run process with given execution context */
  def withScopedEc[Tag] = new WithEcApply[Tag]

  def withEc[F[_]: Execute: FlatMap, A](f: ExecutionContext => F[A]): F[A] = withScopedEc[Main](f)

  def deferScopedFuture[Tag, F[_]] = new DeferFutureApply[Tag, F]

  def deferFuture[F[_]] = new DeferFutureApply[Main, F]

  def deferBlockingFuture[F[_]] = new DeferFutureApply[Blocking, F]

  def deferCalcFuture[F[_]] = new DeferFutureApply[Calculation, F]

}

object ScopedSyntax {
  class ScopedApply[Tag](private val __ : Boolean = true) extends AnyVal {
    def apply[F[_], A](fa: F[A])(implicit F: Scoped[Tag, F]): F[A] = F.runScoped(fa)
  }

  class ScopedDelay[Tag, F[_]](private val __ : Boolean = true) extends AnyVal {
    def apply[A](la: => A)(implicit FS: Scoped[Tag, F], F: Delay[F]): F[A] = FS.runScoped(F.delay(la))
  }

  class DeferFutureApply[Tag, F[_]](private val __ : Boolean = true) extends AnyVal {
    def apply[A](f: ExecutionContext => Future[A])(implicit FS: ScopedExecute[Tag, F]): F[A] =
      FS.deferFutureAction(f)
  }

  class WithEcApply[Tag](private val __ : Boolean = true) extends AnyVal {
    def apply[F[_], A](f: ExecutionContext => F[A])(implicit FS: ScopedExecute[Tag, F], F: FlatMap[F]): F[A] =
      FS.runScoped(FS.executionContext.flatMap(f))
  }
}
