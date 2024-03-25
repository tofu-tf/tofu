package tofu.logging.internal

import cats.{FlatMap, Functor, Monad}
import tofu.Guarantee
import tofu.concurrent.{MakeQVar, QVars}
import tofu.higherKind.{Mid, Post, Pre}
import tofu.lift.Lift
import tofu.logging.impl.{CachedLogs}
import tofu.logging.impl.UniversalEmbedLogs
import tofu.logging.{Logging, Logs}

import scala.reflect.ClassTag
import tofu.syntax.monadic._
import tofu.higherKind.HKAny

object LogsInvariantSyntax {
  final class LogsOps[I[_], F[_]](private val logs: Logs[I, F]) extends AnyVal {
    def cached(implicit IM: Monad[I], IQ: MakeQVar[I, I], IG: Guarantee[I]): I[Logs[I, F]] =
      QVars[I]
        .of(Map.empty[String, Logging[F]])
        .map2(
          QVars[I].of(Map.empty[ClassTag[?], Logging[F]])
        )(new CachedLogs[I, F](logs, _, _))

    def universal(implicit il: Lift[I, F], F: FlatMap[F]): Logs.Universal[F] = new UniversalEmbedLogs(logs)

    @deprecated("Use Logs.universal[F]", since = "0.11.0")
    def cachedUniversal(implicit
        IM: Monad[I],
        IQ: MakeQVar[I, I],
        IG: Guarantee[I],
        il: Lift[I, F],
        F: FlatMap[F]
    ): I[Logs.Universal[F]] = cached.map(_.universal)

    /** Collection of useful methods for creating middleware
      * {{{logs.logged[Service].mid(implicit l => new Service[Mid[F, *]]{... })}}}
      */
    final def logged[U[_[_]]](implicit c: ClassTag[U[HKAny]]): LogWares[U, I, F] =
      new LogWares(logs.forService[U[HKAny]])

    /** Collection of useful methods for creating middleware
      * {{{logs.nameLogged[Service]("service").mid(implicit l => new Service[Mid[F, *]]{... })}}}
      */
    final def nameLogged[U[_[_]]](name: String): LogWares[U, I, F] =
      new LogWares(logs.byName(name))
  }

}

class LogWares[U[_[_]], I[_], F[_]](private val made: I[Logging[F]]) extends AnyVal {
  final def pre(
      make: Logging[F] => U[Pre.Unwrap[F, _]]
  )(implicit I: Functor[I]): I[U[Pre[F, _]]] =
    made.map(logging => Pre.wrap(make(logging)))

  final def post(
      make: Logging[F] => U[Post[F, _]]
  )(implicit I: Functor[I]): I[U[Post[F, _]]] =
    made.map(logging => make(logging))

  final def mid(
      make: Logging[F] => U[Mid[F, _]]
  )(implicit I: Functor[I]): I[U[Mid[F, _]]] =
    made.map(logging => make(logging))
}
