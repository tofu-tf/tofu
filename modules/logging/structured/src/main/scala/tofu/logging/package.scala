package tofu

package object logging {
  @deprecated("Use Logging instead")
  type LoggingBase[F[_]] = Logging[F]

  type ModuleLog[F[_], U[_[_]]] = ServiceLogging[F, U[Any]]
}
