package tofu.doobie.log

import _root_.doobie.LogHandler
import cats.effect.IO
import cats.tagless.FunctorK
import cats.tagless.syntax.functorK._
import cats.{Applicative, FlatMap, Functor, ~>}
import tofu.higherKind.Embed
import tofu.lift.{Lift, UnliftIO}
import tofu.syntax.embed._
import tofu.syntax.monadic._

/** A holder for a [[doobie.util.log.LogHandler]] instance wrapped in an effect `F[_]`.
  * Only useful when `F[_]` is contextual. Allows one to create a context-aware `LogHandler`
  * and embed it into a database algebra that requires it for logging SQL execution events.
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

/** `EmbeddableLogHandler[F]` has two main constructors from `LogHandlerF[F]`: `async` and `sync`.
  * Both require `UnliftIO[F]` and need to perform IO unsafely under the hood due to the impurity
  * of [[doobie.util.log.LogHandler]].
  */
object EmbeddableLogHandler {
  def apply[F[_]](implicit elh: EmbeddableLogHandler[F]): EmbeddableLogHandler[F] = elh

  /** Preferably use `async` as its underlying unsafe run is potentially less harmful.
    */
  def async[F[_]: Functor: UnliftIO](logHandlerF: LogHandlerF[F]): EmbeddableLogHandler[F] =
    fromLogHandlerF(logHandlerF)(_.unsafeRunAsyncAndForget())

  /** Only use `sync` if you are sure your logging logic doesn't contain async computations and
    * cannot block the execution thread.
    */
  def sync[F[_]: Functor: UnliftIO](logHandlerF: LogHandlerF[F]): EmbeddableLogHandler[F] =
    fromLogHandlerF(logHandlerF) { io =>
      io.unsafeRunSync()
      ()
    }

  /** Use `nop` in tests.
    */
  def nop[F[_]: Applicative]: EmbeddableLogHandler[F] = new EmbeddableLogHandler(LogHandler.nop.pure[F])

  private def fromLogHandlerF[F[_]: Functor](
      logHandlerF: LogHandlerF[F]
  )(unsafeRunIO_ : IO[_] => Unit)(implicit U: UnliftIO[F]): EmbeddableLogHandler[F] =
    new EmbeddableLogHandler(U.unlift.map(toIO => LogHandler(event => unsafeRunIO_(toIO(logHandlerF.run(event))))))

  implicit val embeddableLogHandlerFunctorK: FunctorK[EmbeddableLogHandler] = new FunctorK[EmbeddableLogHandler] {
    def mapK[F[_], G[_]](af: EmbeddableLogHandler[F])(fk: F ~> G): EmbeddableLogHandler[G] =
      new EmbeddableLogHandler(fk(af.self))
  }
}
