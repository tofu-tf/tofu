package tofu.env

import monix.eval.Task
import monix.execution.compat.BuildFrom
import tofu.env.internal.CollectionMapper

private[env] trait EnvTraversing {
  def sequence[E, A, M[X] <: Iterable[X]](in: M[Env[E, A]])(
      implicit cbf: BuildFrom[M[Env[E, A]], A, M[A]]): Env[E, M[A]] =
    Env(ctx => Task.traverse(in)(_.run(ctx)))

  def traverse[E, A, B, M[X] <: Iterable[X]](in: M[A])(f: A => Env[E, B])(
      implicit cbf: BuildFrom[M[A], B, M[B]]): Env[E, M[B]] =
    Env(ctx => Task.traverse(in)(x => f(x). run(ctx)))

  def gather[E, A, M[X] <: Iterable[X]](in: M[Env[E, A]])(
      implicit cbf: BuildFrom[M[Env[E, A]], A, M[A]]): Env[E, M[A]] =
    Env(ctx => Task.wander(in)(_.run(ctx)))

  def wander[E, A, B, M[X] <: Iterable[X]](in: M[A])(f: A => Env[E, B])(
      implicit cbf: BuildFrom[M[A], B, M[B]]): Env[E, M[B]] =
    Env(ctx => Task.wander(in)(x => f(x).run(ctx)))

  def gatherUnordered[E, A](in: Iterable[Env[E, A]]): Env[E, List[A]] =
    Env(ctx => Task.wanderUnordered(in)(_.run(ctx)))

  def wanderUnordered[E, A, B, M[X] <: Iterable[X]](in: M[A])(f: A => Env[E, B]): Env[E, List[B]] =
    Env(ctx => Task.wanderUnordered(in)(x => f(x).run(ctx)))

  /** Mirrored traversing operations, trying to create context-unaware Tasks whenever possible */
  object opt {
    type EitherAllTasks[M[_], E, A] = Either[M[Env[E, A]], M[EnvTask[E, A]]]

    @inline private[this] def walkTasks[E, A, M[X] <: Iterable[X]](
        in: M[Env[E, A]],
        fallback: => Env[E, M[A]],
        task: M[Env[E, A]] => (Env[E, A] => Task[A]) => Task[M[A]]): Env[E, M[A]] =
      if (in.forall(_.isInstanceOf[EnvTask[E, A]])) Env.fromTask(task(in)(_.asInstanceOf[EnvTask[E, A]].ta))
      else fallback

    def sequence[E, A, M[+X] <: Iterable[X]](in: M[Env[E, A]])(
        implicit cb: BuildFrom[M[Env[E, A]], A, M[A]]): Env[E, M[A]] =
      walkTasks(in, Env.sequence(in), Task.traverse[Env[E, A], A, M])

    def traverse[E, A, B, M[+X] <: Iterable[X]](in: M[A])(f: A => Env[E, B])(
        implicit mapper : CollectionMapper[A, Env[E, B], M],
        cbf2: BuildFrom[M[Env[E, B]], B, M[B]]): Env[E, M[B]] =  
        sequence(mapper.map(in, f))


    def gather[E, A, M[X] <: Iterable[X]](in: M[Env[E, A]])(
        implicit cbf: BuildFrom[M[Env[E, A]], A, M[A]]): Env[E, M[A]] =
      walkTasks(in, Env.gather(in), Task.wander[Env[E, A], A, M])

    def wander[E, A, B, M[X] <: Iterable[X]](in: M[A])(f: A => Env[E, B])(
        implicit mapper : CollectionMapper[A, Env[E, B], M],
        cbf2: BuildFrom[M[Env[E, B]], B, M[B]]): Env[E, M[B]] = 
        gather(mapper.map(in, f))

    def gatherUnordered[E, A](in: Iterable[Env[E, A]]): Env[E, List[A]] =
      walkTasks(in.toList, Env.gatherUnordered(in), Task.wanderUnordered[Env[E, A], A, List])

    def wanderUnordered[E, A, B, M[X] <: Iterable[X]](in: M[A])(f: A => Env[E, B]): Env[E, List[B]] =
      gatherUnordered(in.map(f))
  }
}
