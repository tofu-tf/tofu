package tofu

import tofu.higherKind.bi.{EmbedBK, FunctorBK}
import tofu.syntax.functorbk._
import tofu.zioInstances.implicits._
import zio.{IO, Tag, ZIO}

object zioFunctions {

  def expose[U[bf[_, _]]: EmbedBK: FunctorBK](implicit ev: Tag[U[IO]]): U[ZIO[U[IO], +_, +_]] =
    EmbedBK.of[ZIO[U[IO], +_, +_], U](ZIO.environmentWith(_.get[U[IO]].widenb))

}
