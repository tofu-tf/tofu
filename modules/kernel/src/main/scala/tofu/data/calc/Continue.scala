package tofu.data.calc

import tofu.compat.unused212
import glass.PContains
import tofu.data.Nothing2T

trait Continue[-A, -E, -S, +C] { self =>
  def success(s: S, a: A): C
  def error(s: S, e: E): C

  def map[D](f: C => D): Continue[A, E, S, D] = new Continue[A, E, S, D] {
    override def success(s: S, a: A): D = f(self.success(s, a))
    override def error(s: S, e: E): D   = f(self.error(s, e))
  }

  def dimap[D, S1](f: C => D, g: S1 => S): Continue[A, E, S1, D] = new Continue[A, E, S1, D] {
    override def success(s: S1, a: A): D = f(self.success(g(s), a))
    override def error(s: S1, e: E): D   = f(self.error(g(s), e))
  }

  def withState[S1 <: S]: Continue[A, E, S1, (S1, C)] = new Continue[A, E, S1, (S1, C)] {
    override def success(s: S1, a: A): (S1, C) = (s, self.success(s, a))
    override def error(s: S1, e: E): (S1, C)   = (s, self.error(s, e))
  }
}

object Continue {
  def apply[A, E, X](f: A => X, h: E => X): Continue[A, E, Any, X] = new Continue[A, E, Any, X] {
    override def success(s: Any, a: A): X = f(a)
    override def error(s: Any, e: E): X   = h(e)
  }

  def of[A, E, S, X](f: (S, A) => X, h: (S, E) => X): Continue[A, E, S, X] = new Continue[A, E, S, X] {
    override def success(s: S, a: A): X = f(s, a)
    override def error(s: S, e: E): X   = h(s, e)
  }

  type Result[A, E, S] = Continue[A, E, Any, CalcM[Nothing2T, Any, S, S, E, A]]

  private[this] def result1[A, E, S]: Result[A, E, S] = new Result[A, E, S] {
    override def success(s: Any, a: A): CalcM[Nothing2T, Any, S, S, E, A] = CalcM.pure(a)
    override def error(s: Any, e: E): CalcM[Nothing2T, Any, S, S, E, A]   = CalcM.raise(e)
  }
  private[this] val resultAny: Result[Any, Any, Any]  = result1
  def result[A, E, S]: Result[A, E, S]                = resultAny.asInstanceOf[Result[A, E, S]]

  type StResult[A, E, S] = Continue[A, E, S, StepResult.Now[S, E, A]]
  private[this] def stepResult1[A, E, S]: StResult[A, E, S] = new StResult[A, E, S] {
    override def success(s: S, a: A) = StepResult.Ok(s, a)
    override def error(s: S, e: E)   = StepResult.Error(s, e)
  }
  private[this] val stepResultAny                           = stepResult1[Any, Any, Any]
  def stepResult[A, E, S]: StResult[A, E, S]                = stepResultAny.asInstanceOf[StResult[A, E, S]]

  def compose[A, B, C, E, V, W, R, S1, S2, S3, F[+_, +_]](
      c1: Continue[A, E, S1, CalcM[F, R, S1, S2, V, B]],
      c2: Continue[B, V, S2, CalcM[F, R, S2, S3, W, C]]
  ): Continue[A, E, S1, CalcM[F, R, S1, S3, W, C]] =
    new Continue[A, E, S1, CalcM[F, R, S1, S3, W, C]] {
      def success(s: S1, a: A): CalcM[F, R, S1, S3, W, C] = c1.success(s, a).bind(c2)
      def error(s: S1, e: E): CalcM[F, R, S1, S3, W, C]   = c1.error(s, e).bind(c2)
    }

  def flatMapConst[A, E, S, X >: CalcM[Nothing2T, Any, S, S, E, Nothing]](f: A => X): Continue[A, E, S, X] =
    new Continue[A, E, S, X] {
      def success(s: S, a: A): X = f(a)
      def error(s: S, e: E): X   = CalcM.Raise[S, E](e)
    }

  def handleWithConst[A, E, S, X >: CalcM[Nothing2T, Any, S, S, Nothing, A]](f: E => X): Continue[A, E, S, X] =
    new Continue[A, E, S, X] {
      def success(s: S, a: A): X = CalcM.Pure[S, A](a)
      def error(s: S, e: E): X   = f(e)
    }

  type Swap[A, E, S] = Continue[A, E, S, CalcM[Nothing2T, Any, S, S, A, E]]
  private[this] def swap1[A, E, S]: Swap[A, E, S] =
    new Swap[A, E, S] {
      override def success(s: S, a: A) = CalcM.Raise(a)
      override def error(s: S, e: E)   = CalcM.Pure(e)
    }

  private[this] val swapAny        = swap1[Any, Any, Any]
  def swap[A, E, S]: Swap[A, E, S] = swapAny.asInstanceOf[Swap[A, E, S]]

  type Update[A, E, SI, SO] = Continue[A, E, SI, CalcM[Nothing2T, Any, Any, SO, E, A]]
  def update[A, E, SI, SO](f: SI => SO): Update[A, E, SI, SO] =
    new Update[A, E, SI, SO] {
      override def success(s: SI, a: A) = CalcM.set(f(s)) as_ a
      override def error(s: SI, e: E)   = CalcM.set(f(s)).swap errorAs_ e
    }

  private[this] def biflatten1[F[+_, +_], R, SI, SO, E, A] =
    new Continue[CalcM[F, R, SI, SO, E, A], CalcM[F, R, SI, SO, E, A], SI, CalcM[F, R, SI, SO, E, A]] {
      def success(s: SI, a: CalcM[F, R, SI, SO, E, A]) = a
      def error(s: SI, e: CalcM[F, R, SI, SO, E, A])   = e
    }
  private[this] val biflattenAny                           = biflatten1[Nothing, Any, Any, Any, Any, Any]

  def biflatten[F[+_, +_], R, SI, SO, E1, A1, E, A](implicit
      @unused212 evA: A1 <:< CalcM[F, R, SI, SO, E, A],
      @unused212 evE: E1 <:< CalcM[F, R, SI, SO, E, A],
  ): Continue[A1, E1, SI, CalcM[F, R, SI, SO, E, A]] =
    biflattenAny.asInstanceOf[Continue[A1, E1, SI, CalcM[F, R, SI, SO, E, A]]]

  def focus[F[+_, +_], R, SI, SO, E, A, S3, S4](s3: S3, lens: PContains[S3, S4, SI, SO]) =
    new Continue[A, E, SO, CalcM[F, R, SO, S4, E, A]] {
      def success(s: SO, result: A): CalcM[F, R, SO, S4, E, A] =
        CalcM.set(lens.set(s3, s)) *>> CalcM.pure(result)
      def error(s: SO, err: E): CalcM[F, R, SO, S4, E, A]      =
        CalcM.set(lens.set(s3, s)) *>> CalcM.raise(err)
    }
}
