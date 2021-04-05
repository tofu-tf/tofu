package tofu.syntax

import tofu.{Fire, Race, Start}
import cats.effect.Fiber

object fire  {
  final implicit class FireOps[F[_], A](private val fa: F[A]) extends AnyVal {
    def fireAndForget(implicit fire: Fire[F]): F[Unit] = fire.fireAndForget(fa)
  }
}
object race  {
  final implicit class RaceOps[F[_], A](private val fa: F[A]) extends AnyVal {
    def race[F1[x] >: F[x], B](fb: F1[B])(implicit F: Race[F1]): F1[Either[A, B]] = F.race(fa, fb)
  }
}
object start {
  final implicit class StartOps[F[_], A](private val fa: F[A]) extends AnyVal {
    def start(implicit F: Start[F]): F[Fiber[F, A]]                                                                    = F.start(fa)
    def racePair[F1[x] >: F[x], B](fb: F1[B])(implicit F: Start[F1]): F1[Either[(A, Fiber[F1, B]), (Fiber[F1, A], B)]] =
      F.racePair(fa, fb)
  }
}
