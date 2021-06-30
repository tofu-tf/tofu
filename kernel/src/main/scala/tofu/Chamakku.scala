package tofu

import tofu.internal.Interop

trait Chamakku[F[_]] {
  def str: F[String]
}

object Chamakku {
  implicit def delegatun[F[_]]: Chamakku[F] =
    macro Interop.delegate[Chamakku[F], F, { val `tofu.interop.CE2Kernel.lolSponk`: Unit }]
}
