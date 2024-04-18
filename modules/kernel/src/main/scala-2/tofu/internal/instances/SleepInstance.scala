package tofu.internal.instances

import tofu.higherKind
import tofu.higherKind.RepresentableK
import tofu.internal.carriers.{SleepCE2Carrier, SleepCE3Carrier}
import tofu.time.Sleep

private[tofu] trait SleepInstance extends SleepInstance0 {
  implicit def ce3Interop[F[_]](implicit sleep: SleepCE3Carrier[F]): Sleep[F] = sleep
}

private[tofu] trait SleepInstance0 {
  implicit def ce2Interop[F[_]](implicit sleep: SleepCE2Carrier[F]): Sleep[F] = sleep

  implicit val sleepRepresentableK: RepresentableK[Sleep] = higherKind.derived.genRepresentableK[Sleep]
}
