package tofu.concurrent.syntax
import tofu.concurrent.{Daemon, Daemonic}

object daemon {
  implicit class TofuDaemonicSyntax[F[_], A](private val process: F[A]) extends AnyVal {
    def daemonize[E](implicit D: Daemonic[F, E]): F[Daemon[F, E, A]] = D.daemonize(process)
  }
}
