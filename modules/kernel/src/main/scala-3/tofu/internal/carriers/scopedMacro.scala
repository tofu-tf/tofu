package tofu.internal
package carriers

import tofu.Scoped

trait ScopedCarrier2Macro:
  implicit inline def asyncExecute[F[_]]: ScopedCarrier2[Scoped.Main, F]       =
    Interop.delegate1[F, ScopedCarrier2[Scoped.Main, F]]("tofu.interop.CE2Kernel.asyncExecute")
  implicit inline def blockerExecute[F[_]]: ScopedCarrier2[Scoped.Blocking, F] =
    Interop.delegate1[F, ScopedCarrier2[Scoped.Blocking, F]]("tofu.interop.CE2Kernel.blockerExecute")

trait ScopedCarrier3Macro:
  implicit inline def asyncExecute[F[_]]: ScopedCarrier3[Scoped.Main, F]       =
    Interop.delegate1[F, ScopedCarrier3[Scoped.Main, F]]("tofu.interop.CE3Kernel.asyncExecute")
  implicit inline def blockerExecute[F[_]]: ScopedCarrier3[Scoped.Blocking, F] =
    Interop.delegate1[F, ScopedCarrier3[Scoped.Blocking, F]]("tofu.interop.CE3Kernel.asyncExecute")
