package tofu
package concurrent
package syntax
import cats.effect.Concurrent
import cats.syntax.parallel._
import cats.{Parallel, Traverse}
import tofu.syntax.monadic._
import cats.effect.implicits._
import cats.effect.std.Semaphore

object traverse {
  implicit final class TraverseOps[T[_], A](val ta: T[A]) extends AnyVal {

    @deprecated("Duplicates cats.effect.syntax.ParallelNSyntax of cats-effect 2.0.0", "0.6.3")
    def limitedTraverse[F[_], B](
        batchSize: Int
    )(f: A => F[B])(implicit T: Traverse[T], F: Concurrent[F]): F[T[B]] =
      for {
        semaphore <- Semaphore[F](batchSize.toLong)
        result    <- ta.parTraverse(value => semaphore.withPermit(f(value)))
      } yield result
  }
}
