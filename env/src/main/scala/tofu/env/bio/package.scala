package tofu.env

import monix.eval.Task

package object bio {
  type BiTask[E, A] = Task[Either[E, A]]
}
