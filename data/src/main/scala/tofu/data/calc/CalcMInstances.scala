package tofu.data.calc

import cats.{Functor, MonadError, Monoid, StackSafeMonad}
import tofu.control.{Bind, StackSafeBind}
import tofu.WithRun
import tofu.bi.BiRun
import cats.Bifunctor
import tofu.higherKind.bi.FunBK

trait CalcMInstances {
  final implicit def calcFunctorInstance[F[+_, +_], R, S, E]: CalcMonadInstance[F, R, S, E] =
    new CalcMonadInstance[F, R, S, E]

  final implicit def calcBindInstance[F[+_, +_], R, S]: CalcBindInstance[F, R, S] =
    new CalcBindInstance[F, R, S]

  final implicit def calcContextInstance[F[+_, +_], R, S, E]: CalcContextInstance[F, R, S, E] =
    new CalcContextInstance[F, R, S, E]

  final implicit def calcBiContextInstance[F[+_, +_], R, S]: CalcBiContextInstance[F, R, S] =
    new CalcBiContextInstance[F, R, S]
}

class CalcMonadInstance[F[+_, +_], R, S, E]
    extends MonadError[CalcM[F, R, S, S, E, *], E] with cats.Defer[CalcM[F, R, S, S, E, *]]
    with StackSafeMonad[CalcM[F, R, S, S, E, *]] {
  def defer[A](fa: => CalcM[F, R, S, S, E, A]): CalcM[F, R, S, S, E, A]                                         = CalcM.defer(fa)
  def raiseError[A](e: E): CalcM[F, R, S, S, E, A]                                                              = CalcM.raise(e)
  def handleErrorWith[A](fa: CalcM[F, R, S, S, E, A])(f: E => CalcM[F, R, S, S, E, A]): CalcM[F, R, S, S, E, A] =
    fa.handleWith(f)
  def flatMap[A, B](fa: CalcM[F, R, S, S, E, A])(f: A => CalcM[F, R, S, S, E, B]): CalcM[F, R, S, S, E, B]      =
    fa.flatMap(f)
  def pure[A](x: A): CalcM[F, R, S, S, E, A]                                                                    = CalcM.pure(x)
}

class CalcBindInstance[F[+_, +_], R, S] extends StackSafeBind[CalcM[F, R, S, S, *, *]] {
  override def pure[E, A](a: A): CalcM[F, R, S, S, E, A] = CalcM.pure(a)

  override def raise[E, A](e: E): CalcM[F, R, S, S, E, A] = CalcM.raise(e)

  override def foldWith[E, A, X, B](
      fa: CalcM[F, R, S, S, E, A],
      h: E => CalcM[F, R, S, S, X, B],
      f: A => CalcM[F, R, S, S, X, B]
  ): CalcM[F, R, S, S, X, B] = fa.foldWith(f, h)
}

class CalcContextInstance[F[+_, +_], R, S, E] extends WithRun[CalcM[F, R, S, S, E, *], CalcM[F, Any, S, S, E, *], R] {
  override val context: CalcM[F, R, S, S, E, R]          = CalcM.read
  override val functor: Functor[CalcM[F, R, S, S, E, *]] = CalcM.calcFunctorInstance

  override def runContext[A](fa: CalcM[F, R, S, S, E, A])(ctx: R): CalcM[F, Any, S, S, E, A]   = fa.provide(ctx)
  override def local[A](fa: CalcM[F, R, S, S, E, A])(project: R => R): CalcM[F, R, S, S, E, A] =
    fa.provideSome(project)
  override def lift[A](fa: CalcM[F, Any, S, S, E, A]): CalcM[F, R, S, S, E, A]                 = fa
}

class CalcBiContextInstance[F[+_, +_], R, S]
    extends BiRun[CalcM[F, R, S, S, *, *], CalcM[F, Any, S, S, *, *], Nothing, R] {
  override def bifunctor: Bind[CalcM[F, R, S, S, *, *]] = CalcM.calcBindInstance

  override def lift[E, A](fa: CalcM[F, Any, S, S, E, A]): CalcM[F, R, S, S, E, A] = fa

  override def runLeft[E, A](fa: CalcM[F, R, S, S, E, A])(x: Nothing): CalcM[F, Any, S, S, E, A] = x

  override def runRight[E, A](fa: CalcM[F, R, S, S, E, A])(r: R): CalcM[F, Any, S, S, E, A] = fa.provide(r)

  override def context: CalcM[F, R, S, S, Nothing, R] = CalcM.read

  override def bilocal[E, A](
      fea: CalcM[F, R, S, S, E, A]
  )(lproj: Nothing => Nothing, rproj: R => R): CalcM[F, R, S, S, E, A] = fea.local(rproj)

  override def disclose[E, A](
      k: FunBK[CalcM[F, R, S, S, *, *], CalcM[F, Any, S, S, *, *]] => CalcM[F, R, S, S, E, A]
  ): CalcM[F, R, S, S, E, A] = CalcM.read[S, R].flatMap(r => k(FunBK.apply(_.provide(r))))
}
