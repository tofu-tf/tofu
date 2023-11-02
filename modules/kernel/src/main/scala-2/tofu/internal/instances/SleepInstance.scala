package tofu.internal.instances

import tofu.time.Sleep
import tofu.internal.carriers.SleepCE3Carrier
import tofu.internal.carriers.SleepCE2Carrier

private[tofu] trait SleepInstance extends SleepInstance0 {
  implicit def ce3Interop[F[_]](implicit sleep: SleepCE3Carrier[F]): Sleep[F] = sleep
}

private[tofu] trait SleepInstance0 {
  implicit def ce2Interop[F[_]](implicit sleep: SleepCE2Carrier[F]): Sleep[F] = sleep
}
