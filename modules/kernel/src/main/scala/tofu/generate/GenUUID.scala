package tofu.generate

import java.util.UUID

import cats.Functor
import cats.syntax.functor._
import tofu.higherKind.RepresentableK
import tofu.internal.EffectComp
import tofu.{Delay, higherKind}

trait GenUUID[F[_]] {
  def randomUUID: F[UUID]
}

object GenUUID extends EffectComp[GenUUID] {
  def random[F[_]](implicit gen: GenUUID[F]): F[UUID] = gen.randomUUID
  def randomString[F[_]: Functor: GenUUID]: F[String] = random[F].map(_.toString)

  implicit def syncGenUUID[F[_]](implicit F: Delay[F]): GenUUID[F] = new GenUUID[F] {
    val randomUUID: F[UUID] = F.delay(UUID.randomUUID())
  }

}
