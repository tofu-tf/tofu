package tofu.doobie.log

import _root_.doobie.LogHandler
import cats.tagless.FunctorK
import cats.tagless.syntax.functorK._
import cats.{Applicative, FlatMap, Functor, ~>}
import tofu.concurrent.Exit
import tofu.higherKind.Embed
import tofu.kernel.types.PerformThrow
import tofu.lift.Lift
import tofu.syntax.embed._
import tofu.syntax.monadic._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Promise}

/** A holder for a [[doobie.util.log.LogHandler]] instance wrapped in an effect `F[_]`. Only useful when `F[_]` is
  * contextual. Allows one to create a context-aware `LogHandler` and embed it into a database algebra that requires it
  * for logging SQL execution events.
  */
final class EmbeddableLogHandler[F[_]](val self: F[LogHandler]) extends AnyVal {
  def embedMapK[A[_[_]]: Embed: FunctorK, G[_]](impl: LogHandler => A[G])(fk: G ~> F)(implicit F: FlatMap[F]): A[F] =
    self.map(impl(_).mapK(fk)).embed

  def embedLift[A[_[_]]: Embed: FunctorK, G[_]](
      impl: LogHandler => A[G]
  )(implicit F: FlatMap[F], L: Lift[G, F]): A[F] = embedMapK(impl)(L.liftF)

  def embed[A[_[_]]: Embed](impl: LogHandler => A[F])(implicit F: FlatMap[F]): A[F] =
    self.map(impl).embed
}

/** `EmbeddableLogHandler[F]` has two main constructors from `LogHandlerF[F]`: `async` and `sync`. Both require
  * `UnliftIO[F]` and need to perform IO unsafely under the hood due to the impurity of `doobie.util.log.LogHandler`.
  */
object EmbeddableLogHandler {
  def apply[F[_]](implicit elh: EmbeddableLogHandler[F]): EmbeddableLogHandler[F] = elh

  /** Preferably use `async` as its underlying unsafe run is potentially less harmful.
    */
  def async[F[_]: Functor](logHandlerF: LogHandlerF[F])(implicit P: PerformThrow[F]): EmbeddableLogHandler[F] =
    new EmbeddableLogHandler(P.performer.map { perf =>
      LogHandler { event =>
        val _ = perf.perform((exit: Exit[Throwable, Unit]) =>
          exit match {
            case Exit.Canceled     => throw Exit.CanceledException
            case Exit.Error(e)     => throw e
            case Exit.Completed(_) => ()
          }
        )(logHandlerF.run(event))
      }
    })

  /** Only use `sync` if you are sure your logging logic doesn't contain async computations and cannot block the
    * execution thread.
    */
  def sync[F[_]: Functor](logHandlerF: LogHandlerF[F])(implicit P: PerformThrow[F]): EmbeddableLogHandler[F] =
    new EmbeddableLogHandler(P.performer.map { performer =>
      LogHandler { event =>
        val promise = Promise[Unit]()
        val _       = performer.perform((exit: Exit[Throwable, Unit]) =>
          exit match {
            case Exit.Canceled     => promise.failure(Exit.CanceledException)
            case Exit.Error(e)     => promise.failure(e)
            case Exit.Completed(_) => promise.success(())
          }
        )(logHandlerF.run(event))
        Await.result(promise.future, Duration.Inf)
      }
    })

  /** Use `nop` in tests.
    */
  def nop[F[_]: Applicative]: EmbeddableLogHandler[F] = new EmbeddableLogHandler(LogHandler.nop.pure[F])

  implicit val embeddableLogHandlerFunctorK: FunctorK[EmbeddableLogHandler] = new FunctorK[EmbeddableLogHandler] {
    def mapK[F[_], G[_]](af: EmbeddableLogHandler[F])(fk: F ~> G): EmbeddableLogHandler[G] =
      new EmbeddableLogHandler(fk(af.self))
  }
}
