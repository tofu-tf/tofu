package tofu.fs2Instances

import cats.data.ReaderT
import cats.effect.IO
import fs2.Stream
import tofu.WithContext
import tofu.optics.macros.{ClassyOptics, promote}

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

  import configs.AppConfig._

  type RunF[A] = ReaderT[IO, configs.AppConfig, A]
  type StreamF[+A] = Stream[RunF, A]

  final class Service[S[_]: configs.SubConfigA.Has, F[_]: configs.SubConfigB.Has]

  def make = new Service[RunF, StreamF]
}
