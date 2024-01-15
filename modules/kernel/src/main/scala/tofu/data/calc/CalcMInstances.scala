package tofu.data.calc

import cats.{Applicative, Bifoldable, Bitraverse, Eval, Functor, MonadError, StackSafeMonad}
import tofu.WithRun
import tofu.bi.BiRun
import tofu.control.{Bind, StackSafeBind}
import tofu.data.calc.StepResult._
import tofu.higherKind.bi.FunBK
import tofu.syntax.monadic._

trait CalcMInstances extends CalcMInstances1 {
  final implicit def calcFunctorInstance[F[+_, +_], R, S, E]: CalcMonadInstance[F, R, S, E] =
    new CalcMonadInstance[F, R, S, E]

  final implicit def calcBindInstance[F[+_, +_], R, S]: CalcBindInstance[F, R, S] =
    new CalcBindInstance[F, R, S]

  final implicit def calcContextInstance[F[+_, +_], R, S, E]: CalcContextInstance[F, R, S, E] =
    new CalcContextInstance[F, R, S, E]

  final implicit def calcBiContextInstance[F[+_, +_], R, S]: CalcBiContextInstance[F, R, S] =
    new CalcBiContextInstance[F, R, S]

}

trait CalcMInstances1 {
  final implicit def calcMBitraverse[F[+_, +_]: Bitraverse, S]: Bitraverse[CalcM[F, Any, Any, S, *, *]] =
    new CalcMBitraverse
}

trait CalcMInstances2 {
  final implicit def calcMBifoldable[F[+_, +_]: Bifoldable, S]: Bifoldable[CalcM[F, Any, Any, S, *, *]] =
    new CalcMBifoldable
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

  override def foldWith[E, A, X, B](fa: CalcM[F, R, S, S, E, A])(
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
    extends BiRun[CalcM[F, R, S, S, +*, +*], CalcM[F, Any, S, S, +*, +*], Nothing, R] {
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
  ): CalcM[F, R, S, S, E, A] = CalcM.read[S, R].flatMap(r => k(FunBK.apply[CalcM[F, R, S, S, *, *]](_.provide(r))))
}

class CalcMBifoldable[F[+_, +_], S](implicit F: Bifoldable[F]) extends Bifoldable[CalcM[F, Any, Any, S, *, *]] {
  private[this] val MaxDepth = 256

  def bifoldLeft1Slow[E, A, B](
      c: StepResult[F, S, E, A],
      b: Eval[B]
  )(f: (Eval[B], E) => Eval[B], g: (Eval[B], A) => Eval[B]): Eval[B] =
    Eval.defer(c match {
      case Error(_, e)                     => f(b, e)
      case Ok(_, a)                        => g(b, a)
      case w: Wrap[F, r, s, S, x, E, m, A] =>
        F.bifoldLeft(w.inner, b)(
          (b1, x) => bifoldLeft1Slow(w.stepFailure(x), b1)(f, g),
          (b1, m) => bifoldLeft1Slow(w.stepSuccess(m), b1)(f, g)
        )
    })

  def bifoldLeft1[E, A, B](c: StepResult[F, S, E, A], b: B, depth: Int)(f: (B, E) => B, g: (B, A) => B): B = {
    if (depth == MaxDepth)
      bifoldLeft1Slow(c, Eval.now(b))(
        (eb, e) => eb.map(f(_, e)),
        (eb, a) => eb.map(g(_, a))
      ).value
    else
      c match {
        case Error(_, e)                     => f(b, e)
        case Ok(_, a)                        => g(b, a)
        case w: Wrap[F, r, s, S, x, E, m, A] =>
          F.bifoldLeft(w.inner, b)(
            (b1, x) => bifoldLeft1(w.stepFailure(x), b1, depth + 1)(f, g),
            (b1, m) => bifoldLeft1(w.stepSuccess(m), b1, depth + 1)(f, g)
          )
      }
  }

  def bifoldLeft[A, B, C](fab: CalcM[F, Any, Any, S, A, B], c: C)(f: (C, A) => C, g: (C, B) => C): C =
    bifoldLeft1(fab.step((), ()), c, 0)(f, g)

  def bifoldRight1[E, A, B](c: StepResult[F, S, E, A], b: Eval[B])(
      f: (E, Eval[B]) => Eval[B],
      g: (A, Eval[B]) => Eval[B]
  ): Eval[B] = c match {
    case Error(_, e)                     => f(e, b)
    case Ok(_, a)                        => g(a, b)
    case w: Wrap[F, r, s, S, x, E, m, A] =>
      F.bifoldRight(w.inner, b)(
        (x, b1) => Eval.defer(bifoldRight1(w.stepFailure(x), b1)(f, g)),
        (m, b1) => Eval.defer(bifoldRight1(w.stepSuccess(m), b1)(f, g))
      )
  }

  def bifoldRight[A, B, C](fab: CalcM[F, Any, Any, S, A, B], c: Eval[C])(
      f: (A, Eval[C]) => Eval[C],
      g: (B, Eval[C]) => Eval[C]
  ): Eval[C] = bifoldRight1(fab.step((), ()), c)(f, g)
}

class CalcMBitraverse[F[+_, +_], S](implicit F: Bitraverse[F])
    extends CalcMBifoldable[F, S] with Bitraverse[CalcM[F, Any, Any, S, *, *]] {

  def bitraverse1[G[_]: Applicative, A, B, C, D](
      fab: StepResult[F, S, A, B]
  )(f: A => G[C], g: B => G[D]): G[CalcM[F, Any, Any, S, C, D]] = fab match {
    case Error(s, a)                     =>
      f(a).map(c => CalcM.set(s) >> CalcM.raise(c))
    case Ok(s, b)                        =>
      g(b).map(d => CalcM.set(s) >> CalcM.pure(d))
    case w: Wrap[F, r, s, S, x, A, m, B] =>
      F.bitraverse(w.inner)(
        x => bitraverse1(w.stepFailure(x))(f, g),
        m => bitraverse1(w.stepSuccess(m))(f, g)
      ).map(CalcM.roll)
  }

  def bitraverse[G[_]: Applicative, A, B, C, D](
      fab: CalcM[F, Any, Any, S, A, B]
  )(f: A => G[C], g: B => G[D]): G[CalcM[F, Any, Any, S, C, D]] = ???
}
