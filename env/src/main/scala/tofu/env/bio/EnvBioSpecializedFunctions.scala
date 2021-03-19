package tofu.env.bio

import monix.eval.Task

trait EnvBioSpecializedFunctions[R, E] {
  type F[A] = EnvBio[R, E, A]

  def apply[A](f: R => Task[Either[E, A]]): EnvBio[R, E, A] = EnvBio.apply(f)
  def applyFatal[A](f: R => Task[A]): EnvBio[R, E, A]       = EnvBio.applyFatal(a => f(a))
  def pure[A](x: A): EnvBio[R, E, A]                        = EnvBio.pure(x)
  def raiseError(e: E): EnvBio[R, E, Nothing]               = EnvBio.raiseError(e)

  def context: EnvBio[R, E, R]                               = EnvBio.context
  def fromTask[A](task: Task[A]): EnvBio[R, Throwable, A]    = EnvBio.fromTask(task)
  def fromTaskTotal[A](task: Task[A]): EnvBio[R, Nothing, A] = EnvBio.fromTaskTotal(task)

  def map2[A, B, C, R1 <: R, E1 >: E](ea: EnvBio[R, E, A], eb: EnvBio[R1, E1, B])(f: (A, B) => C): EnvBio[R1, E1, C] =
    EnvBio.map2(ea, eb)(f)

  def map3[A, B, C, D, R1 <: R, E1 >: E](ea: EnvBio[R, E, A], eb: EnvBio[R1, E1, B], ec: EnvBio[R1, E1, C])(
      f: (A, B, C) => D
  ): EnvBio[R1, E1, D] = EnvBio.map3(ea, eb, ec)(f)

  def parMap2[A, B, C, R1 <: R, E1 >: E](ea: EnvBio[R, E, A], eb: EnvBio[R1, E1, B])(
      f: (A, B) => C
  ): EnvBio[R1, E1, C] = EnvBio.parMap2(ea, eb)(f)

  def parMap3[A, B, C, D, R1 <: R, E1 >: E](ea: EnvBio[R, E, A], eb: EnvBio[R1, E1, B], ec: EnvBio[R1, E1, C])(
      f: (A, B, C) => D
  ): EnvBio[R1, E1, D] = EnvBio.parMap3(ea, eb, ec)(f)
}
