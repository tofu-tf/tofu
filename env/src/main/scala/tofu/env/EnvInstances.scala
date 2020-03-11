package tofu.env

import cats.arrow.{ArrowChoice, FunctionK, Profunctor}
import cats.{Applicative, Monad, Parallel, ~>}
import monix.eval.Task
import monix.execution.Scheduler
import tofu.lift.UnsafeExecFuture
import tofu.optics.Contains
import tofu.syntax.funk._

import scala.concurrent.Future

private[env] trait EnvInstances {
  self: Env.type =>

  private object anyEnvInstance extends EnvFunctorstance[Any]

  final implicit def envInstance[E]: EnvFunctorstance[E] = anyEnvInstance.asInstanceOf[EnvFunctorstance[E]]

  private object envParallelInstance extends Applicative[Env[Any, *]] {
    override def pure[A](x: A): Env[Any, A] = Env.pure(x)
    override def ap[A, B](ff: Env[Any, A => B])(fa: Env[Any, A]): Env[Any, B] =
      Env.parMap2(ff, fa)(_.apply(_))
    override def map2[A, B, Z](fa: Env[Any, A], fb: Env[Any, B])(f: (A, B) => Z): Env[Any, Z] =
      Env.parMap2(fa, fb)(f)
    override val unit: Env[Any, Unit]                               = Env.unit
    override def map[A, B](fa: Env[Any, A])(f: A => B): Env[Any, B] = fa.map(f)
    override def replicateA[A](n: Int, fa: Env[Any, A]): Env[Any, List[A]] =
      fa.mapTask(t => Task.gather(Iterable.fill(n)(t)).map(_.toList))
  }

  private object anyEnvParallelInstance extends Parallel[Env[Any, *]] {
    type F[a] = Env[Any, a]
    override def applicative: Applicative[Env[Any, *]]    = envParallelInstance
    override def monad: Monad[Env[Any, *]]                = anyEnvInstance
    override val sequential: ~>[Env[Any, *], Env[Any, *]] = FunctionK.id
    override val parallel: ~>[Env[Any, *], Env[Any, *]]   = FunctionK.id
  }

  final implicit def envParallelInstance[E]: Parallel[Env[E, *]] =
    anyEnvParallelInstance.asInstanceOf[Parallel[Env[E, *]]]

  final implicit val envProfuctorInstance: Profunctor[Env] with ArrowChoice[Env] =
    new Profunctor[Env] with ArrowChoice[Env] {
      override def choose[A, B, C, D](f: Env[A, C])(g: Env[B, D]): Env[Either[A, B], Either[C, D]] =
        Env {
          case Left(a)  => f.run(a).map(Left(_))
          case Right(b) => g.run(b).map(Right(_))
        }

      override def lift[A, B](f: A => B): Env[A, B] = Env(a => Task.pure(f(a)))
      override def first[A, B, C](fa: Env[A, B]): Env[(A, C), (B, C)] =
        fa.first[C]
      override def second[A, B, C](fa: Env[A, B]): Env[(C, A), (C, B)] =
        fa.second[C]
      override def compose[A, B, C](f: Env[B, C], g: Env[A, B]): Env[A, C] =
        f.compose(g)
      override def rmap[A, B, C](fab: Env[A, B])(f: B => C): Env[A, C] =
        fab.map(f)
      override def lmap[A, B, C](fab: Env[A, B])(f: C => A): Env[C, B] =
        fab.localP(f)
      override def id[A]: Env[A, A]                                                   = Env.context
      override def dimap[A, B, C, D](fab: Env[A, B])(f: C => A)(g: B => D): Env[C, D] = fab.dimap(f)(g)
      override def split[A, B, C, D](f: Env[A, B], g: Env[C, D]): Env[(A, C), (B, D)] =
        f.split(g)
      override def left[A, B, C](fab: Env[A, B]): Env[Either[A, C], Either[B, C]]  = fab.left[C]
      override def right[A, B, C](fab: Env[A, B]): Env[Either[C, A], Either[C, B]] = fab.right[C]
      override def choice[A, B, C](f: Env[A, C], g: Env[B, C]): Env[Either[A, B], C] =
        f.choice(g)
      override def merge[A, B, C](f: Env[A, B], g: Env[A, C]): Env[A, (B, C)] =
        Env.parZip2(f, g)
    }

  private[this] val envUnliftAny = new EnvUnliftTask[Any]

  final implicit def envUnliftTask[E]: EnvUnliftTask[E] = envUnliftAny.asInstanceOf[EnvUnliftTask[E]]

  final implicit def envUnliftSubContext[E, E1: E Contains *]: EnvUnliftSubContext[E, E1] = new EnvUnliftSubContext

  def envUnsafeExecFuture[E](implicit sc: Scheduler): UnsafeExecFuture[Env[E, *]] =
    new UnsafeExecFuture[Env[E, *]] {
      def lift[A](fa: Future[A]): Env[E, A]   = Env.fromFuture(fa)
      def unlift: Env[E, Env[E, *] ~> Future] = Env.fromFunc(r => makeFunctionK(_.run(r).runToFuture))
    }
}
