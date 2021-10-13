package tofu.internal
package carriers

import tofu.ScopedExecute
import tofu.Scoped

trait ScopedCarrier2[T, F[_]] extends ScopedExecute[T, F]

object ScopedCarrier2 {
  final implicit def asyncExecute[F[_]]: ScopedCarrier2[Scoped.Main, F] =
    macro Interop.delegate[ScopedCarrier2[Scoped.Main, F], F, { val `tofu.interop.CE2Kernel.asyncExecute`: Unit }]

  final implicit def blockerExecute[F[_]]: ScopedCarrier2[Scoped.Blocking, F] =
    macro Interop.delegate[ScopedCarrier2[Scoped.Blocking, F], F, { val `tofu.interop.CE2Kernel.blockerExecute`: Unit }]
}

trait ScopedCarrier3[T, F[_]] extends ScopedExecute[T, F]

object ScopedCarrier3 {
  final implicit def asyncExecute[F[_]]: ScopedCarrier3[Scoped.Main, F] =
    macro Interop.delegate[ScopedCarrier3[Scoped.Main, F], F, { val `tofu.interop.CE3Kernel.asyncExecute`: Unit }]

  final implicit def blockerExecute[F[_]]: ScopedCarrier3[Scoped.Blocking, F] =
    macro Interop.delegate[ScopedCarrier3[Scoped.Blocking, F], F, { val `tofu.interop.CE3Kernel.blockerExecute`: Unit }]
}
