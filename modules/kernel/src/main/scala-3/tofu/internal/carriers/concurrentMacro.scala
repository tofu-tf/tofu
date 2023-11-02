package tofu.internal.carriers

import tofu.internal.Interop

trait MkAtomCE2CarrierMacro:
  inline given interopCE2Carrier[I[_], F[_]]: MkAtomCE2Carrier[I, F] =
    Interop.delegate2[I, F, MkAtomCE2Carrier[I, F]]("tofu.interop.CE2Kernel.atomBySync")

trait MkAtomCE3CarrierMacro:
  inline given interopCE3Carrier[I[_], F[_]]: MkAtomCE3Carrier[I, F] =
    Interop.delegate2[I, F, MkAtomCE3Carrier[I, F]]("tofu.interop.CE3Kernel.atomBySync")

trait MkQVarCE2CarrierMacro:
  inline given interopCE2Carrier[I[_], F[_]]: MkQVarCE2Carrier[I, F] =
    Interop.delegate2[I, F, MkQVarCE2Carrier[I, F]]("tofu.interop.CE2Kernel.qvarByConcurrent")

trait MkQVarCE3CarrierMacro:
  inline given interopCE3Carrier[I[_], F[_]]: MkQVarCE3Carrier[I, F] =
    Interop.delegate2[I, F, MkQVarCE3Carrier[I, F]]("tofu.interop.CE3Kernel.qvarByConcurrent")

trait MkAgentCE2CarrierMacro:
  inline given interopCE2Carrier[I[_], F[_]]: MkAgentCE2Carrier[I, F] =
    Interop.delegate2[I, F, MkAgentCE2Carrier[I, F]]("tofu.interop.CE2Kernel.agentByRefAndSemaphore")

trait MkAgentCE3CarrierMacro:
  inline given interopCE3Carrier[I[_], F[_]]: MkAgentCE3Carrier[I, F] =
    Interop.delegate2[I, F, MkAgentCE3Carrier[I, F]]("tofu.interop.CE3Kernel.agentByRefAndSemaphore")

trait MkSerialAgentCE2CarrierMacro extends MkSerialAgentCE2Carrier0Macro:
  inline given interopCE2Carrier[I[_], F[_]]: MkSerialAgentCE2Carrier[I, F] =
    Interop.delegate2[I, F, MkSerialAgentCE2Carrier[I, F]]("tofu.interop.CE2Kernel.serialAgentByRefAndSemaphore")

trait MkSerialAgentCE2Carrier0Macro:
  inline given underlyinginteropCE2Carrier[I[_], F[_]]: MkSerialAgentCE2Carrier[I, F] =
    Interop.delegate2[I, F, MkSerialAgentCE2Carrier[I, F]](
      "tofu.interop.CE2Kernel.underlyingSerialAgentByRefAndSemaphore"
    )

trait MkSerialAgentCE3CarrierMacro extends MkSerialAgentCE3Carrier0Macro:
  inline given interopCE3Carrier[I[_], F[_]]: MkSerialAgentCE3Carrier[I, F] =
    Interop.delegate2[I, F, MkSerialAgentCE3Carrier[I, F]]("tofu.interop.CE3Kernel.serialAgentByRefAndSemaphore")

trait MkSerialAgentCE3Carrier0Macro:
  inline given underlyinginteropCE3Carrier[I[_], F[_]]: MkSerialAgentCE3Carrier[I, F] =
    Interop.delegate2[I, F, MkSerialAgentCE3Carrier[I, F]](
      "tofu.interop.CE3Kernel.underlyingSerialAgentByRefAndSemaphore"
    )
