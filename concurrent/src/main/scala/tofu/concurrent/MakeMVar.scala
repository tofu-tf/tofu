package tofu.concurrent

import cats.effect.concurrent.{MVar, MVar2}
import cats.effect.{Concurrent, Sync}

trait MakeMVar[I[_], F[_]] {
  def mvarOf[A](a: A): I[MVar2[F, A]]
  def mvarEmpty[A]: I[MVar2[F, A]]
}

object MVars {
  def apply[F[_]](implicit agents: MVars[F]): MakeMVar.Applier[F, F] = new MakeMVar.Applier[F, F](agents)
}

object MakeMVar {
  def apply[I[_], F[_]](implicit mkvar: MakeMVar[I, F]) = new Applier[I, F](mkvar)

  final class Applier[I[_], F[_]](private val makeMVar: MakeMVar[I, F]) extends AnyVal {
    def empty[A]: I[MVar2[F, A]]    = makeMVar.mvarEmpty[A]
    def of[A](a: A): I[MVar2[F, A]] = makeMVar.mvarOf(a)
  }

  implicit def concurrentMakeMVar[I[_]: Sync, F[_]: Concurrent]: MakeMVar[I, F] = new MakeMVar[I, F] {
    def mvarOf[A](a: A): I[MVar2[F, A]] = MVar.in[I, F, A](a)
    def mvarEmpty[A]: I[MVar2[F, A]]    = MVar.emptyIn[I, F, A]
  }
}
