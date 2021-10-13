package tofu.memo

import cats.{Monad, ~>}
import cats.syntax.applicative._
import cats.syntax.functor._
import cats.syntax.option._
import cats.tagless.FunctorK

sealed trait CacheOperation[F[_], A, B] {
  def tryGet(cval: CacheVal[A])(implicit F: Monad[F]): Option[B]
  def update(cval: CacheVal[A])(implicit F: Monad[F]): F[(CacheVal[A], B)]

  def getPureOrElse(cval: CacheVal[A])(orElse: => F[B])(implicit F: Monad[F]): F[B] =
    tryGet(cval).fold(orElse)(_.pure[F])

  def mapK[G[_]](f: F ~> G): CacheOperation[G, A, B]
}

object CacheOperation {
  final case class GetOrElse[F[_], A](process: F[A], now: Long, after: Long) extends CacheOperation[F, A, A]       {
    override def tryGet(cval: CacheVal[A])(implicit F: Monad[F]): Option[A]           =
      cval.filter(after).getOption
    override def update(cval: CacheVal[A])(implicit F: Monad[F]): F[(CacheVal[A], A)] =
      cval.filter(after) match {
        case CacheVal.Some(_, cached) => (cval, cached).pure[F]
        case CacheVal.None            => process.map(a => (CacheVal.some(now, a), a))
      }
    def mapK[G[_]](f: F ~> G): CacheOperation[G, A, A]                                = GetOrElse(f(process), now, after)
  }
  final case class CleanUp[F[_], A](after: Long)                             extends CacheOperation[F, A, Boolean] {
    override def tryGet(cval: CacheVal[A])(implicit F: Monad[F]): Option[Boolean]           =
      if (cval.expired(after)) none else false.some
    override def update(cval: CacheVal[A])(implicit F: Monad[F]): F[(CacheVal[A], Boolean)] =
      (if (cval.expired(after)) (CacheVal.none[A], true) else (cval, false)).pure[F]

    def mapK[G[_]](f: F ~> G): CacheOperation[G, A, Boolean] = CleanUp(after)
  }

  final implicit def functorK[A, B]: FunctorK[CacheOperation[*[_], A, B]] =
    new FunctorK[CacheOperation[*[_], A, B]] {
      def mapK[F[_], G[_]](af: CacheOperation[F, A, B])(fk: F ~> G): CacheOperation[G, A, B] = af.mapK(fk)
    }
}
