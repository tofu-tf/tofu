package tofu

import izumi.reflect.Tag
import tofu.higherKind.bi.{EmbedBK, FunctorBK}
import tofu.syntax.functorbk._
import tofu.zioInstances.implicits._
import zio.{IO, ZIO, Tag => ZTag}

import scala.annotation.nowarn

object zioFunctions {
  @nowarn("cat=unused-params")
  def expose[U[_[_, _]]: EmbedBK: FunctorBK: Tag.auto.T]: U[ZIO[U[IO], +_, +_]] =
    EmbedBK.of[ZIO[U[IO], +_, +_], U](ZIO.environmentWith(_.get[U[IO]](ZTag[U[IO]]).widenb))
}
