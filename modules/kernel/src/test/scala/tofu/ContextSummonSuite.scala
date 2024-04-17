package tofu

import cats.Monad
import cats.data.ReaderT
import tofu.lift.Unlift

object ContextSummonSuite {

  class Ctx
  type R[F[_], x] = ReaderT[F, Ctx, x]

  def summonReaderTComposeInstances[F[_], G[_]: Monad](implicit U: Unlift[F, G]): Any = {
    implicitly[Unlift[F, R[G, _]]]
  }

  def summonReaderTWrappedInstances1[F[_], G[_], C](implicit WR: WithRun[G, F, C]): Any = {
    implicitly[WithContext[R[G, _], C]]
    implicitly[WithLocal[R[G, _], C]]
    implicitly[WithRun[R[G, _], R[F, _], C]]
    implicitly[Unlift[R[F, _], R[G, _]]]
  }

  def summonReaderTWrappedInstances2[F[_], G[_], C](implicit WP: WithProvide[G, F, C]): Any = {
    implicitly[WithProvide[R[G, _], R[F, _], C]]
  }

  def summonReaderTWrappedInstances3[F[_], C](implicit WL: WithLocal[F, C]): Any = {
    implicitly[WithContext[R[F, _], C]]
    implicitly[WithLocal[R[F, _], C]]
  }

  def summonReaderTWrappedInstances4[F[_], C](implicit WC: WithContext[F, C]): Any = {
    implicitly[WithContext[R[F, _], C]]
  }

}
