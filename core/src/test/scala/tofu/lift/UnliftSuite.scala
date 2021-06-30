package tofu.lift

import cats.Applicative
import cats.data.ReaderT
import cats.effect.{Effect, IO}
import cats.syntax.option._
import org.scalatest.flatspec.AnyFlatSpec
import tofu.compat.unused
import cats.Monad

class UnliftSuite extends AnyFlatSpec {
  "Lift implicit def implementations" should "cast instances properly" in {
    assert(
      implicitly[Lift[Option, Option]].lift(42.some) === Some(42)
    )
    assert(
      implicitly[Lift[Option, ReaderT[Option, String, *]]].lift(42.some).run("x") === Some(42)
    )
  }
}

object UnliftSuite {

  def summonLiftInstances[F[_], R](): Unit = {
    implicitly[Lift[F, F]]
    implicitly[Lift[F, ReaderT[F, R, *]]]
    ()
  }

  def summonLiftWithIsoKUnambiguously[F[_]](implicit @unused iso: IsoK[F, F]): Unit = {
    implicitly[Lift[F, F]]
    ()
  }

  def summonLiftByIsoK1[F[_], G[_]](implicit iso: IsoK[F, G]): Unit = {
    implicitly[Lift[F, G]]
    implicitly[Lift[G, F]]
    ()
  }

  def summonLiftByIsoK2[F[_], G[_]](implicit iso1: IsoK[F, G], iso2: IsoK[G, F]): Unit = {
    implicitly[Lift[F, G]]
    implicitly[Lift[G, F]]
    ()
  }

  def summonUnliftInstances[F[_]: Applicative, R](): Unit = {
    implicitly[Unlift[F, F]]
    implicitly[Unlift[F, ReaderT[F, R, *]]]
    ()
  }

  def summonUnliftIOInstances1[F[_]: Effect, R](): Unit = {
    implicitly[UnliftIO[F]]
    implicitly[UnliftIO[ReaderT[F, R, *]]]
    implicitly[Unlift[F, ReaderT[F, R, *]]]
    ()
  }

  def summonUnliftIOInstances2[R](): Unit = {
    implicitly[UnliftIO[ReaderT[IO, R, *]]]
    implicitly[UnliftIO[IO]]
    ()
  }

  def summonUnliftIOInstances3[F[_]: UnliftIO: Monad, R](): Unit = {
    implicitly[UnliftIO[ReaderT[F, R, *]]]
    ()
  }

}
