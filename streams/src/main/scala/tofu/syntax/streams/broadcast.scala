package tofu.syntax.streams

import tofu.streams.Broadcast

object broadcast {

  implicit final class BroadcastOps[F[_], A](private val fa: F[A]) extends AnyVal {

    def broadcast(processors: F[A] => F[Unit]*)(implicit ev: Broadcast[F]): F[Unit] =
      ev.broadcast(fa)(processors: _*)

    def broadcast(maxConcurrent: Int)(processor: F[A] => F[Unit])(implicit ev: Broadcast[F]): F[Unit] =
      ev.broadcast(fa)(List.fill(maxConcurrent)(processor): _*)

    def broadcastThrough[B](processors: F[A] => F[B]*)(implicit ev: Broadcast[F]): F[B] =
      ev.broadcastThrough(fa)(processors: _*)

    def broadcastThrough[B](maxConcurrent: Int)(processor: F[A] => F[B])(implicit ev: Broadcast[F]): F[B] =
      ev.broadcastThrough(fa)(List.fill(maxConcurrent)(processor): _*)
  }
}
