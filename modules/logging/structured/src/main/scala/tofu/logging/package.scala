package tofu

import tofu.higherKind.HKAny

package object logging {
  type ModuleLog[F[_], U[_[_]]] = ServiceLogging[F, U[HKAny]]
}
