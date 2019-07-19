package tofu.parallel

import cats._
import cats.arrow.FunctionK
import simulacrum.typeclass

@typeclass
trait Paralleled[F[_]] extends Splitting[F] { self =>
  override type Par[a]
  override def parallel[a](fa: F[a]): Par[a]
  override def sequential[a](pa: Par[a]): F[a]
  def monad: Monad[F]
  def applicative: Applicative[Par]

  override def flatMap: FlatMap[F] = monad
  override def apply: Apply[Par]   = applicative

  def parSequence[T[_], A](ta: T[F[A]])(implicit T: Traverse[T]): F[T[A]] =
    sequential(T.traverse[Par, F[A], A](ta)(a => parallel(a))(applicative))

  def parSequence_[T[_], A](ta: T[F[A]])(implicit T: Foldable[T]): F[Unit] =
    sequential(T.traverse_[Par, F[A], A](ta)(a => parallel(a))(applicative))

  def parTraverse[T[_], A, B](ta: T[A])(f: A => F[B])(implicit T: Traverse[T]): F[T[B]] =
    sequential(T.traverse[Par, A, B](ta)(a => parallel(f(a)))(applicative))

  def parTraverse_[T[_], A, B](ta: T[A])(f: A => F[B])(implicit T: Foldable[T]): F[Unit] =
    sequential(T.traverse_[Par, A, B](ta)(a => parallel(f(a)))(applicative))

  override def toCats: Parallel[F, Par] = new Parallel[F, Par] {
    def applicative: Applicative[Par] = self.applicative
    def monad: Monad[F]               = self.monad
    def sequential: Par ~> F          = FunctionK.lift[Par, F](self.sequential)
    def parallel: F ~> Par            = FunctionK.lift[F, Par](self.parallel)
  }
}

object Paralleled {
  type Aux[F[_], G[_]] = Paralleled[F] { type Par[a] = G[a] }

  implicit def fromCats[F[_], G[_]](implicit p: Parallel[F, G]): Aux[F, G] = new Paralleled[F] {
    override type Par[a] = G[a]
    override def parallel[a](fa: F[a]): G[a]   = p.parallel(fa)
    override def sequential[a](pa: G[a]): F[a] = p.sequential(pa)
    override def monad: Monad[F]               = p.monad
    override def applicative: Applicative[G]   = p.applicative
  }
}
