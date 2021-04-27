package tofu

import tofu.higherKind.{Mid, Point, PureK}
import cats._
import tofu.syntax.funk._
import tofu.syntax.monadic._
import scala.concurrent.{ExecutionContext, Future}
import cats.effect.{ContextShift, Async}
import cats.effect.Blocker

/** can be used for scoped transformations
  * @tparam Tag arbitrary type tag for discriminating scopes
  * @tparam F process type
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

  /**  a helper for creating new instances of `Scoped`
    *  {{{
    *  val instance: Scoped[Tag, F] = Scoped.make[Tag, F](fa => ...)
    *  }}}
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
    * case class MyContext(field: Field, ...)
    * val newField: Field = ???
    * type My[+A] = Env[MyContext, A]
    * type Updated
    * implicit val myScoped: Scoped[Updated, My] = Scoped.local[MyContext](_.copy(field = newField))}
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

trait ScopedInstances {
  final def makeExecute[Tag, F[_]](
      ec: ExecutionContext
  )(implicit cs: ContextShift[F], F: Async[F]): ScopedExecute[Tag, F] =
    new ScopedExecute[Tag, F] {
      def runScoped[A](fa: F[A]): F[A] = cs.evalOn(ec)(fa)

      def executionContext: F[ExecutionContext] = ec.pure[F]

      def deferFutureAction[A](f: ExecutionContext => Future[A]): F[A] =
        Async.fromFuture(runScoped(F.delay(f(ec))))
    }

  final implicit def asyncExecute[F[_]](implicit
      ec: ExecutionContext,
      cs: ContextShift[F],
      F: Async[F]
  ): Execute[F] = makeExecute[Scoped.Main, F](ec)

  final implicit def blockerExecute[F[_]](implicit
      cs: ContextShift[F],
      blocker: Blocker,
      F: Async[F]
  ): BlockExec[F] = makeExecute[Scoped.Blocking, F](blocker.blockingContext)
}

object Execute {
  def apply[F[_]](implicit F: Execute[F]): F.type = F
}

object Blocks {
  def apply[F[_]](implicit F: Blocks[F]): F.type = F
}

object BlockExec {
  def apply[F[_]](implicit F: BlockExec[F]): F.type = F
}

object Calculates {
  def apply[F[_]](implicit F: Calculates[F]): F.type = F
}

object CalcExec {
  def apply[F[_]](implicit F: CalcExec[F]): F.type = F
}
