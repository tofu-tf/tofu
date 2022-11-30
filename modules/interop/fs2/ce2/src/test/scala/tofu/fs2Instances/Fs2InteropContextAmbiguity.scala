package tofu.fs2Instances

import cats.data.ReaderT
import cats.effect.IO
import fs2.Stream
import tofu.WithContext
import glass.macros.{ClassyOptics, promote}
import scala.annotation.nowarn

object Fs2InteropContextAmbiguity {

  object configs {
    @ClassyOptics
    case class AppConfig(a: Int, @promote b: SubConfigA, @promote c: SubConfigB)
    object AppConfig extends WithContext.Companion[AppConfig]

    case class SubConfigA(x: Int, y: String)
    object SubConfigA extends WithContext.Companion[SubConfigA]

    case class SubConfigB(x: Int, y: String)
    object SubConfigB extends WithContext.Companion[SubConfigB]
  }

  import configs._

  type RunF[A]     = ReaderT[IO, AppConfig, A]
  type StreamF[+A] = Stream[RunF, A]

  @nowarn("cat=unused")
  final class Service[S[_]: SubConfigA.Has, F[_]: SubConfigB.Has]

  def make = new Service[RunF, StreamF]
}
