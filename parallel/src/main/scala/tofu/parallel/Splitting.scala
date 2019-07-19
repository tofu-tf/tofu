package tofu.parallel

import cats._
import cats.arrow.FunctionK
import simulacrum.{op, typeclass}

@typeclass
trait Splitting[F[_]] { self =>
  type Par[a]
  def parallel[a](fa: F[a]): Par[a]
  def sequential[a](pa: Par[a]): F[a]
  def flatMap: FlatMap[F]
  def apply: Apply[Par]

  def parMap2[A, B, C](fa: F[A])(fb: F[B])(f: (A, B) => C): F[C] =
    sequential(apply.map2(parallel(fa), parallel(fb))(f))

  def parMap3[A, B, C, D](fa: F[A])(fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    sequential(apply.map3(parallel(fa), parallel(fb), parallel(fc))(f))

  def parMap4[A, B, C, D, E](fa: F[A])(fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] =
    sequential(apply.map4(parallel(fa), parallel(fb), parallel(fc), parallel(fd))(f))

  def parMap5[A1, A2, A3, A4, A5, R](fa: F[A1])(fa2: F[A2], fa3: F[A3], fa4: F[A4], fa5: F[A5])(
      f: (A1, A2, A3, A4, A5) => R): F[R] =
    sequential(apply.map5(parallel(fa), parallel(fa2), parallel(fa3), parallel(fa4), parallel(fa5))(f))

  def parMap6[A1, A2, A3, A4, A5, A6, R](fa: F[A1])(fa2: F[A2], fa3: F[A3], fa4: F[A4], fa5: F[A5], fa6: F[A6])(
      f: (A1, A2, A3, A4, A5, A6) => R): F[R] =
    sequential(apply.map6(parallel(fa), parallel(fa2), parallel(fa3), parallel(fa4), parallel(fa5), parallel(fa6))(f))

  @op("&>", alias = true)
  def parProductR[A, B](fa: F[A])(fb: F[B]): F[B] =
    sequential(apply.productR(parallel(fa))(parallel(fb)))

  @op("<&", alias = true)
  def parProductL[A, B](fa: F[A])(fb: F[B]): F[A] =
    sequential(apply.productL(parallel(fa))(parallel(fb)))

  def parProduct[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    sequential(apply.product(parallel(fa), parallel(fb)))

  @op("<&>", alias = true)
  def parAp[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    sequential(apply.ap(parallel(fab))(parallel(fa)))

  def parNonEmptySequence[T[_], A](ta: T[F[A]])(implicit T: NonEmptyTraverse[T]): F[T[A]] =
    sequential(T.nonEmptyTraverse[Par, F[A], A](ta)(a => parallel(a))(apply))

  def parNonEmptySequence_[T[_], A](ta: T[F[A]])(implicit T: Reducible[T]): F[Unit] =
    sequential(T.nonEmptyTraverse_[Par, F[A], A](ta)(a => parallel(a))(apply))

  def parNonEmptyTraverse[T[_], A, B](ta: T[A])(f: A => F[B])(implicit T: NonEmptyTraverse[T]): F[T[B]] =
    sequential(T.nonEmptyTraverse[Par, A, B](ta)(a => parallel(f(a)))(apply))

  def parNonEmptyTraverse_[T[_], A, B](ta: T[A])(f: A => F[B])(implicit T: Reducible[T]): F[Unit] =
    sequential(T.nonEmptyTraverse_[Par, A, B](ta)(a => parallel(f(a)))(apply))

  def toCats: NonEmptyParallel[F, Par] = new NonEmptyParallel[F, Par] {
    def apply: Apply[Par]    = self.apply
    def flatMap: FlatMap[F]  = self.flatMap
    def sequential: Par ~> F = FunctionK.lift[Par, F](self.sequential)
    def parallel: F ~> Par   = FunctionK.lift[F, Par](self.parallel)
  }
}

object Splitting {
  type Aux[F[_], G[_]] = Splitting[F] { type Par[a] = G[a] }

  implicit def fromCats[F[_], G[_]](implicit p: NonEmptyParallel[F, G]): Aux[F, G] = new Splitting[F] {
    override type Par[a] = G[a]
    override def parallel[a](fa: F[a]): G[a]   = p.parallel(fa)
    override def sequential[a](pa: G[a]): F[a] = p.sequential(pa)
    override def flatMap: FlatMap[F]           = p.flatMap
    override def apply: Apply[G]               = p.apply
  }
}
