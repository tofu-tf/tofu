package tofu.fs2Instances

import cats.tagless.FunctorK
import fs2._
import tofu._
import tofu.higherKind.Embed

object Fs2InstancesSuite {

  def summonFs2HKInstances[A](): Unit = {
    implicitly[FunctorK[Stream[*[_], A]]]
    implicitly[Embed[Stream[*[_], A]]]
    ()
  }

  def summonFs2ContextInstances[R, F[_]: *[_] WithContext R](): Unit = {
    implicitly[WithContext[Stream[F, *], R]]
    ()
  }

  def summonFs2LocalInstances[R, F[_]: *[_] WithLocal R](): Unit = {
    implicitly[WithLocal[Stream[F, *], R]]
    ()
  }

  def summonFs2ProvideInstances[R, G[_], F[_]: WithProvide[*[_], G, R]](): Unit = {
    implicitly[WithProvide[Stream[F, *], Stream[G, *], R]]
    ()
  }

  def summonFs2RunInstances[R, G[_], F[_]: WithRun[*[_], G, R]](): Unit = {
    implicitly[WithRun[Stream[F, *], Stream[G, *], R]]
    ()
  }

}
