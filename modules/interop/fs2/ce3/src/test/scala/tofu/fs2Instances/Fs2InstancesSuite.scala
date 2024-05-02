package tofu.fs2Instances

import cats.tagless.FunctorK
import fs2._
import tofu._
import tofu.higherKind.Embed

import scala.annotation.nowarn

object Fs2InstancesSuite {

  def summonFs2HKInstances[A](): Unit = {
    @nowarn val x = implicitly[FunctorK[({ type L[x[_]] = Stream[x, A] })#L]]
    @nowarn val y = implicitly[Embed[({ type L[x[_]] = Stream[x, A] })#L]]
    ()
  }

  def summonFs2ContextInstances[R, F[_]: ({ type L[x[_]] = WithContext[x, R] })#L](): Unit = {
    implicitly[WithContext[Stream[F, *], R]]
    ()
  }

  def summonFs2LocalInstances[R, F[_]: ({ type L[x[_]] = WithLocal[x, R] })#L](): Unit = {
    implicitly[WithLocal[Stream[F, *], R]]
    ()
  }

  def summonFs2ProvideInstances[R, G[_], F[_]: ({ type L[x[_]] = WithProvide[x, G, R] })#L](): Unit = {
    implicitly[WithProvide[Stream[F, _], Stream[G, _], R]]
    ()
  }

  def summonFs2RunInstances[R, G[_], F[_]: ({ type L[x[_]] = WithRun[x, G, R] })#L](): Unit = {
    implicitly[WithRun[Stream[F, _], Stream[G, _], R]]
    ()
  }

}
