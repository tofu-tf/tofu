package tofu.logging
import cats.Functor
import tofu.syntax.monadic._

import scala.reflect.ClassTag

trait LogsVOps[I[_], F[_]]
