package tofu
package concurrent
import tofu.lift.Lift
import tofu.syntax.lift._

object SyntaxCheck {
  def liftDaemon[F[_], G[_], E, A](daemon: Daemon[F, E, A])(implicit lift: Lift[F, G]): Daemon[G, E, A] =
    daemon.lift2[G]
}
