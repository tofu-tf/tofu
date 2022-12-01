package tofu

import cats.{Applicative, Functor, Monoid}
import glass.Folded
import tofu.internal.DataEffectComp
import glass.classes.Transform

/** simply function A => F[Unit] in a typeclass form */
trait Push[F[_], A] {
  def push(a: A): F[Unit]
}

object Push extends PushInstances with DataEffectComp[Push]

trait PushInstances extends PushInstances1 {
  final implicit def pushTransformed[F[_], A, B](implicit push: Push[F, B], transform: Transform[A, B]): Push[F, A] =
    a => push.push(transform(a))
}

trait PushInstances1 {
  final implicit def pushFolded[F[_], A, B](implicit
      push: Push[F, B],
      fold: Folded[A, B],
      F: Applicative[F]
  ): Push[F, A] = {
    implicit val monoid: Monoid[F[Unit]] = new Monoid[F[Unit]] {
      def empty: F[Unit]                           = F.unit
      def combine(x: F[Unit], y: F[Unit]): F[Unit] = F.productR(x)(y)
    }

    fold.foldMap(_)(push.push)
  }
}

/** simply F[A] in a typeclass form */
trait Pull[F[_], A] {
  def pull: F[A]
}

object Pull extends PullInstances with DataEffectComp[Pull]
trait PullInstances {
  final implicit def pullTransformed[F[_], A, B](implicit
      p: Pull[F, A],
      F: Functor[F],
      transform: Transform[A, B]
  ): Pull[F, B] =
    new Pull[F, B] {
      def pull: F[B] = F.map(p.pull)(transform.apply)
    }
}
