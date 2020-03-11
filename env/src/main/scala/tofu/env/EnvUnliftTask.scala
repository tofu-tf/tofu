package tofu.env
import cats.~>
import monix.eval.Task
import tofu.lift.Unlift
import tofu.optics.Contains
import tofu.syntax.funk.funK

class EnvUnliftTask[E] extends Unlift[Task, Env[E, *]] {
  def lift[A](fa: Task[A]): Env[E, A]   = Env.fromTask(fa)
  def unlift: Env[E, Env[E, *] ~> Task] = Env.fromFunc(r => funK(_.run(r)))
}

class EnvUnliftSubContext[E, E1](implicit e1InE: E Contains E1) extends Unlift[Env[E1, *], Env[E, *]] {
  def lift[A](fa: Env[E1, A]): Env[E, A] = fa.localP(e1InE.extract)
  def unlift: Env[E, Env[E, *] ~> Env[E1, *]] =
    Env.fromFunc(e => funK(_.localP(e1InE.set(e, _))))
}
