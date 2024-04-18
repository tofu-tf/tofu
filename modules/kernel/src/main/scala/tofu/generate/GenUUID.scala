package tofu.generate

import cats.data.*
import cats.syntax.functor.*
import cats.{Applicative, FlatMap, Functor, Monoid}
import tofu.Delay
import tofu.internal.EffectComp
import tofu.syntax.liftKernel.CatsTaglessLiftSyntax

import java.util.UUID

trait GenUUID[F[_]] {
  def randomUUID: F[UUID]
}

object GenUUID extends EffectComp[GenUUID] with GenUUIDInstances {
  def random[F[_]](implicit gen: GenUUID[F]): F[UUID] = gen.randomUUID
  def randomString[F[_]: Functor: GenUUID]: F[String] = random[F].map(_.toString)

  implicit def syncGenUUID[F[_]](implicit F: Delay[F]): GenUUID[F] = new GenUUID[F] {
    val randomUUID: F[UUID] = F.delay(UUID.randomUUID())
  }

  implicit def genUUIDForKleisli[F[_]: GenUUID, R]: GenUUID[Kleisli[F, R, _]]                            = GenUUID[F].lift
  implicit def genUUIDForWriterT[F[_]: Applicative: GenUUID, R: Monoid]: GenUUID[WriterT[F, R, _]]       = GenUUID[F].lift
  implicit def genUUIDForOptionT[F[_]: Functor: GenUUID]: GenUUID[OptionT[F, _]]                         = GenUUID[F].lift
  implicit def genUUIDForEitherT[F[_]: Functor: GenUUID, E]: GenUUID[EitherT[F, E, _]]                   = GenUUID[F].lift
  implicit def genUUIDForStateT[F[_]: Applicative: GenUUID, S]: GenUUID[StateT[F, S, _]]                 = GenUUID[F].lift
  implicit def genUUIDForIorT[F[_]: Applicative: GenUUID, L]: GenUUID[IorT[F, L, _]]                     = GenUUID[F].lift
  implicit def genUUIDForContT[F[_]: FlatMap: GenUUID, R]: GenUUID[ContT[F, R, _]]                       = GenUUID[F].lift
  implicit def genUUIDForRWST[F[_]: Applicative: GenUUID, R, L: Monoid, S]: GenUUID[RWST[F, R, L, S, _]] =
    GenUUID[F].lift
}
