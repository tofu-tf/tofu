package tofu.control

import cats.data.{Chain, WriterT}
import cats.effect.IO

object SelectiveSummon {
  Selective[WriterT[IO, Chain[Int], *]]
}
