package tofu

import tofu.higherKind.{Mid, Point, PureK}
import cats._
import tofu.syntax.funk._
import scala.concurrent.{ExecutionContext, Future}
import tofu.internal.EffectComp
import tofu.kernel.types._
import tofu.internal.carriers.ScopedCarrier2
import tofu.internal.carriers.ScopedCarrier3

/** can be used for scoped transformations
  * @tparam Tag
  *   arbitrary type tag f type Execute[F[_]] = ScopedExecute[Scoped.Main, F]
  *
  * type Blocks[F[_]] = Scoped[Scoped.Blocking, F] type BlockExec[F[_]] = ScopedExecute[Scoped.Blocking, F]
  *
  * type Calculates[F[_]] = Scoped[Scoped.Calculation, F] type CalcExec[F[_]] = ScopedExecute[Scoped.Calculation, F]or
  * discriminating scopes
  * @tparam F
  *   process type
  */
trait Scoped[Tag, F[_]] {
  def runScoped[A](fa: F[A]): F[A]

  final def asMid[A]: Mid[F, A] = fa => fa

  final def funK: F ~> F = funKFrom[F](fa => fa)

  final def tagged[NewTag]: Scoped[NewTag, F] = this.asInstanceOf[Scoped[NewTag, F]]

  final def midPoint: Point[Mid[F, *]] = new Point[Mid[F, *]] {
    def point[A]: Mid[F, A] = runScoped
  }
}

object Scoped extends ScopedInstances {

  /** type tag for blocking scopes */
  type Blocking

  /** type tag for resource-consuming calculations */
  type Calculation

  /** type tag for the main scope */
  type Main

  def apply[Tag, F[_]](implicit sc: Scoped[Tag, F]): Scoped[Tag, F] = sc

  /** a helper for creating new instances of `Scoped`
    * {{{
    *   val instance: Scoped[Tag, F] = Scoped.make[Tag, F](fa => ...)
    * }}}
    */
  def make[Tag, F[_]] = new Make[Tag, F]

  class Make[Tag, F[_]](private val __ : Boolean = true) extends AnyVal         {
    type Arbitrary
    def apply(maker: Maker[Tag, F, Arbitrary]): Scoped[Tag, F] = maker
  }
  abstract class Maker[Tag, F[_], Arbitrary]             extends Scoped[Tag, F] {
    def arbApply(fa: F[Arbitrary]): F[Arbitrary]
    final def runScoped[A](fa: F[A]): F[A] = arbApply(fa.asInstanceOf[F[Arbitrary]]).asInstanceOf[F[A]]
  }

  /** could be used to define scopes with modified environment
    * {{{
    * case class MyContext(field: Field, ...) val newField:
    * Field = ??? type My[+A] = Env[MyContext, A] type Updated implicit val myScoped: Scoped[Updated, My] =
    * Scoped.local[MyContext](_.copy(field = newField))
    * }}}
    */
  def local[C]: Local[C] = new Local

  class Local[C](private val __ : Boolean = true) extends AnyVal {
    def apply[Tag, F[_]](transform: C => C)(implicit FL: F WithLocal C): Scoped[Tag, F] =
      make[Tag, F](FL.local(_)(transform))
  }

  /** helpful method to create middleware that executes all proceses in the given scope */
  def mid[Tag, U[_[_]], F[_]](implicit U: PureK[U], F: Scoped[Tag, F]): U[Mid[F, *]] = U.pureK(F.midPoint)
}

trait ScopedExecute[Tag, F[_]] extends Scoped[Tag, F] {
  def executionContext: F[ExecutionContext]

  def deferFutureAction[A](f: ExecutionContext => Future[A]): F[A]

  def deferFuture[A](f: => Future[A]): F[A] = deferFutureAction(_ => f)
}

trait ScopedInstances extends ScopedInstances0 with ScopedInstancesMacro {

  final implicit def interopCE3[Tag, F[_]](implicit carrier: ScopedCarrier3[Tag, F]): ScopedExecute[Tag, F] = carrier
}

private[tofu] trait ScopedInstances0 {
  final implicit def interopCE2[Tag, F[_]](implicit carrier: ScopedCarrier2[Tag, F]): ScopedExecute[Tag, F] = carrier
}

object Execute extends EffectComp[Execute]

object Blocks extends EffectComp[Blocks]

object BlockExec extends EffectComp[BlockExec]

object Calculates extends EffectComp[Calculates]

object CalcExec extends EffectComp[CalcExec]
