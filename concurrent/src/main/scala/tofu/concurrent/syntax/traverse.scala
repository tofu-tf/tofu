package tofu
package concurrent
package syntax
import cats.{Parallel, Traverse}
import cats.effect.Concurrent
import cats.effect.concurrent.Semaphore
import cats.syntax.parallel._
import cats.syntax.flatMap._
import cats.syntax.functor._

object traverse {
  implicit class TraverseOps[T[_], A](val ta: T[A]) extends AnyVal {
    def limitedTraverse[F[_], B](
        batchSize: Int
    )(f: A => F[B])(implicit T: Traverse[T], F: Concurrent[F], P: Parallel[F]): F[T[B]] =
      for {
        semaphore <- Semaphore[F](batchSize.toLong)
        result    <- ta.parTraverse(value => semaphore.withPermit(f(value)))
      } yield result
  }
}
