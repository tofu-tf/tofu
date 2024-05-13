package tofu

import izumi.reflect.Tag
import tofu.compat.unused
import tofu.higherKind.bi.{EmbedBK, FunctorBK}
import tofu.syntax.functorbk._
import tofu.zioInstances.implicits._
import zio.{IO, Tag => ZTag, ZIO}

object zioFunctions {
  @unused
  def expose[U[_[_, _]]: EmbedBK: FunctorBK: Tag.auto.T]: U[ZIO[U[IO], +_, +_]] =
    EmbedBK.of[ZIO[U[IO], +_, +_], U](ZIO.environmentWith(_.get[U[IO]](ZTag[U[IO]]).widenb))
}
