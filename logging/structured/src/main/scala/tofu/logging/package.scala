package tofu

package object logging {
  type ModuleLog[F[_], U[_[_]]] = ServiceLogging[F, U[Any]]
}
