package tofu

import tofu.higherKind.bi.EmbedBK
import zio.ZIO
import zio.Has
import zio.IO
import syntax.functorbk._
import zioInstances.implicits._
import tofu.higherKind.bi.FunctorBK
import izumi.reflect.HKTag
import scala.annotation.nowarn

object zioFunctions {
  type TagB[U[f[_, _]]] = HKTag[{ type Arg[f[_, _]] = U[f] }]
  @nowarn
  def expose[U[bf[_, _]]: EmbedBK: FunctorBK: TagB]: U[ZIO[Has[U[IO]], +*, +*]] =
    EmbedBK.of[ZIO[Has[U[IO]], +*, +*], U](ZIO.access(_.get[U[IO]].widenb))
}
