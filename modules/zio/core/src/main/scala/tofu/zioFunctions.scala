package tofu

import izumi.reflect.HKTag
import tofu.higherKind.bi.{EmbedBK, FunctorBK}
import zio.{Has, IO, ZIO}

import scala.annotation.nowarn

import syntax.functorbk._
import zioInstances.implicits._

object zioFunctions {
  type TagB[U[f[_, _]]] = HKTag[{ type Arg[f[_, _]] = U[f] }]
  @nowarn
  def expose[U[bf[_, _]]: EmbedBK: FunctorBK: TagB]: U[ZIO[Has[U[IO]], +*, +*]] =
    EmbedBK.of[ZIO[Has[U[IO]], +*, +*], U](ZIO.access(_.get[U[IO]].widenb))
}
