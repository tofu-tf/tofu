package tofu.syntax

import tofu.{Fire, Race, Fibers}

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
    def start[Exit[_], Fib[_]](implicit F: Fibers[F, Exit, Fib]): F[Fib[A]] = F.start(fa)
    def racePair[F1[x] >: F[x], B, Exit[_], Fib[_]](fb: F1[B])(implicit
        F: Fibers[F1, Exit, Fib]
    ): F1[Either[(Exit[A], Fib[B]), (Fib[A], Exit[B])]] =
      F.racePair[A, B](fa, fb)
  }
}
