package tofu.logging

import _root_.zio.{UIO, URIO}

package object zio {
  type ZLogs[R]    = Logs[UIO, URIO[R, *]]
  type ZLogging[R] = Logging[URIO[R, *]]
}
