package tofu.env.bio

import monix.eval.Task

trait EnvBioSpecializedFunctions[R, E] {
  type F[A] = EnvBio[R, E, A]

  def apply[A](f: R => Task[Either[E, A]]): EnvBio[R, E, A] = EnvBio.apply(f)
  def applyFatal[A](f: R => Task[A]): EnvBio[R, E, A]       = EnvBio.applyFatal(f)
  def later[A](x: => A): EnvBio[R, E, A]                    = EnvBio.later(x)
  def pure[A](x: A): EnvBio[R, E, A]                        = EnvBio.pure(x)
  def raiseError(e: E): EnvBio[R, E, Nothing]               = EnvBio.raiseError(e)

  def context: EnvBio[R, E, R]                    = EnvBio.context
  def fromTask[A](task: Task[A]): EnvBio[R, E, A] = EnvBio.fromTask(task)
}
