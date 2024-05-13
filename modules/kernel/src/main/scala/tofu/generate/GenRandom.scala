package tofu.generate

import cats.data.*
import cats.{Applicative, FlatMap, Functor, Monoid}
import tofu.Delay
import tofu.internal.EffectComp
import tofu.syntax.liftKernel.CatsTaglessLiftSyntax
import tofu.syntax.monadic.*

import scala.util.Random

trait GenRandom[F[_]] {

  /** generate random 8-bit number */
  def nextLong: F[Long]

  /** Returns a pseudorandom, uniformly distributed int value between 0 (inclusive) and the specified value (exclusive),
    * drawn from this random number generator's sequence.
    */
  def nextInt(n: Int): F[Int]
}

object GenRandom extends EffectComp[GenRandom] with GetRandomInstances {
  def nextLong[F[_]](implicit g: GenRandom[F]): F[Long]       = g.nextLong
  def nextInt[F[_]](n: Int)(implicit g: GenRandom[F]): F[Int] = g.nextInt(n)

  def instance[I[_]: Delay: Functor, F[_]: Delay](
      seed: Option[Long] = None,
      secure: Boolean = false
  ): I[GenRandom[F]] = {
    def createStd()    = seed.fold(new java.util.Random)(new java.util.Random(_))
    def createSecure() = {
      val rnd = new java.security.SecureRandom()
      seed.foreach(rnd.setSeed)
      rnd
    }

    def random(): java.util.Random = if (secure) createSecure() else createStd()
    for (rnd <- Delay[I].delay(new Random(random()))) yield new ScalaUtil[F](rnd)
  }

  private class ScalaUtil[F[_]](rnd: Random)(implicit F: Delay[F]) extends GenRandom[F] {
    def nextLong: F[Long]         = F.delay(rnd.nextLong())
    def nextInt(max: Int): F[Int] = F.delay(rnd.nextInt(max))
  }

  implicit def GenRandomForKleisli[F[_]: GenRandom, R]: GenRandom[Kleisli[F, R, _]]                            = GenRandom[F].lift
  implicit def GenRandomForOptionT[F[_]: Functor: GenRandom]: GenRandom[OptionT[F, _]]                         = GenRandom[F].lift
  implicit def GenRandomForEitherT[F[_]: Functor: GenRandom, E]: GenRandom[EitherT[F, E, _]]                   = GenRandom[F].lift
  implicit def GenRandomForStateT[F[_]: Applicative: GenRandom, S]: GenRandom[StateT[F, S, _]]                 = GenRandom[F].lift
  implicit def GenRandomForIorT[F[_]: Applicative: GenRandom, L]: GenRandom[IorT[F, L, _]]                     = GenRandom[F].lift
  implicit def GenRandomForContT[F[_]: FlatMap: GenRandom, R]: GenRandom[ContT[F, R, _]]                       = GenRandom[F].lift
  implicit def GenRandomForWriterT[F[_]: Applicative: GenRandom, R: Monoid]: GenRandom[WriterT[F, R, _]]       =
    GenRandom[F].lift
  implicit def GenRandomForRWST[F[_]: Applicative: GenRandom, R, L: Monoid, S]: GenRandom[RWST[F, R, L, S, _]] =
    GenRandom[F].lift
}
