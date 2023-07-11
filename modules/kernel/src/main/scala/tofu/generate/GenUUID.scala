package tofu.generate

import cats.Functor
import cats.syntax.functor.*
import tofu.Delay
import tofu.internal.EffectComp

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

}
