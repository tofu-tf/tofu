package tofu.logging

import Logging.loggerForService
import cats.data.Tuple2K
import cats.effect.Sync
import cats.kernel.Monoid
import cats.{Applicative, Apply, FlatMap, Functor, Id, Monad}
import impl.{CachedLogs, ContextSyncLoggingImpl, SyncLogging, UniversalEmbedLogs}
import cats.tagless.{ApplyK, FunctorK}
import cats.tagless.syntax.functorK._
import cats.{Applicative, Apply, FlatMap, Functor, ~>}
import impl.{ContextSyncLoggingImpl, SyncLogging}
import org.slf4j.LoggerFactory
import tofu.concurrent.QVars
import tofu.{Guarantee, higherKind}
import tofu.higherKind.RepresentableK
import tofu.lift.Lift
import tofu.higherKind
import tofu.higherKind.{Function2K, MonoidalK, Point, RepresentableK}
import tofu.syntax.monadic._
import tofu.syntax.monoidalK._

import scala.reflect.ClassTag

/**
  * A helper for creating instances of [[tofu.logging.Logging]], defining a way these instances will behave while doing logging.
  * Can create instances either on a by-name basic or a type tag basic.
  * An instance of [[tofu.logging.Logs]] can be shared between different pieces of code depending
  * on whether logging behaviour should be shared.
  * However it's not uncommon to use different [[Logs]] for different parts of program.
  *
  * Sample usage would be:
  * {{{
  *   val logs: Logs[F, F] = Logs.sync[F, F]
  *
  *   def program[F[_]: Sync] =
  *     for {
  *       logging <- logs.byName("my-logger")
  *       _       <- logging.info("this is a message")
  *       _       <- logging.info("this is another message")
  *     } yield ()
  * }}}
  */
trait Logs[+I[_], F[_]] extends LogsVOps[I, F] {

  /** Creates an instance of [[tofu.logging.Logging]] with a given arbitrary type tag,
    * using it as a class to create underlying [[org.slf4j.Logger]] with. */
  def forService[Svc: ClassTag]: I[Logging[F]]

  /** Creates an instance of [[tofu.logging.Logging]] for a given arbitrary string name,
    * using it to create underlying [[org.slf4j.Logger]] with.
    */
  def byName(name: String): I[Logging[F]]

  final def biwiden[I1[a] >: I[a], F1[a] >: F[a]]: Logs[I1, F1] = this.asInstanceOf[Logs[I1, F1]]

  final def service[Svc: ClassTag]: I[ServiceLogging[F, Svc]] = forService[Svc].asInstanceOf[I[ServiceLogging[F, Svc]]]

}

object Logs extends LogsInstances0 {
  type Universal[F[_]] = Logs[Id, F]

  def apply[I[_], F[_]](implicit logs: Logs[I, F]): Logs[I, F] = logs

  private[this] val logs1RepresentableAny: RepresentableK[Logs[*[_], Any]] =
    higherKind.derived.genRepresentableK[Logs[*[_], Any]]

  implicit def logs1Representable[Y[_]]: RepresentableK[Logs[*[_], Y]] =
    logs1RepresentableAny.asInstanceOf[RepresentableK[Logs[*[_], Y]]]

  implicit def logs2MonoidalK[Y[_]](implicit Y: Applicative[Y]): MonoidalK[Logs[Y, *[_]]] =
    new Logs2MonoidalK[Y] { def I: Applicative[Y] = Y }

  def provide[I[_], F[_]]  = new Provide[I, F]
  def provideM[I[_], F[_]] = new ProvideM[I, F]

  /**
    * Returns an instance of [[tofu.logging.Logs]] that requires [[cats.effect.Sync]] to perform logging side-effects.
    * Has no notion of context.
    */
  def sync[I[_]: Sync, F[_]: Sync]: Logs[I, F] = new Logs[I, F] {
    def forService[Svc: ClassTag]: I[Logging[F]] =
      Sync[I].delay(new SyncLogging[F](loggerForService[Svc]))
    def byName(name: String): I[Logging[F]] = Sync[I].delay(new SyncLogging[F](LoggerFactory.getLogger(name)))
  }

  def withContext[I[_]: Sync, F[_]: Sync](implicit ctx: LoggableContext[F]): Logs[I, F] = {
    import ctx.loggable
    new Logs[I, F] {
      def forService[Svc: ClassTag]: I[Logging[F]] =
        Sync[I].delay(new ContextSyncLoggingImpl[F, ctx.Ctx](ctx.context, loggerForService[Svc]))
      override def byName(name: String): I[Logging[F]] =
        Sync[I].delay(new ContextSyncLoggingImpl[F, ctx.Ctx](ctx.context, LoggerFactory.getLogger(name)))
    }
  }

  /**
    * Returns an instance of [[tofu.logging.Logs]] that will produce the same constant [[tofu.logging.Logging]] instances.
    */
  def const[I[_]: Applicative, F[_]](logging: Logging[F]): Logs[I, F] = new Logs[I, F] {
    def forService[Svc: ClassTag]: I[Logging[F]] = logging.pure[I]
    def byName(name: String): I[Logging[F]]      = logging.pure[I]
  }

  /**
    * Returns an instance of [[tofu.logging.Logs]] that will produce a no-op [[tofu.logging.Logging]] instances.
    */
  def empty[I[_]: Applicative, F[_]: Applicative]: Logs[I, F] = const[I, F](Logging.empty[F])

  /**
    * Combines two instances of [[tofu.logging.Logs]], resulting in a single one that will produce
    * instances of [[tofu.logging.Logging]] by combining.
    * Resulting [[tofu.logging.Logging]] instance will call both implementations in order.
    */
  def combine[I[_]: Apply, F[_]: Apply](las: Logs[I, F], lbs: Logs[I, F]): Logs[I, F] = new Logs[I, F] {
    def forService[Svc: ClassTag]: I[Logging[F]] = las.forService.map2(lbs.forService)(Logging.combine[F])
    def byName(name: String): I[Logging[F]]      = las.byName(name).map2(lbs.byName(name))(Logging.combine[F])
  }

  implicit def logsMonoid[I[_]: Applicative, F[_]: Applicative]: Monoid[Logs[I, F]] = new Monoid[Logs[I, F]] {
    def empty: Logs[I, F]                                 = Logs.empty[I, F]
    def combine(x: Logs[I, F], y: Logs[I, F]): Logs[I, F] = Logs.combine(x, y)
  }

  class Provide[I[_], F[_]] {
    def apply[X: ClassTag](f: Logging[F] => X)(implicit logs: Logs[I, F], I: Functor[I]) = logs.forService[X].map(f)
  }
  class ProvideM[I[_], F[_]] {
    def apply[X: ClassTag](f: Logging[F] => I[X])(implicit logs: Logs[I, F], I: FlatMap[I]) =
      logs.forService[X].flatMap(f)
  }

  final implicit class LogsOps[I[_], F[_]](private val logs: Logs[I, F]) extends AnyVal {
    def cached(implicit IM: Monad[I], IQ: QVars[I], IG: Guarantee[I]): I[Logs[I, F]] =
      QVars[I]
        .of(Map.empty[String, Logging[F]])
        .map2(
          QVars[I].of(Map.empty[ClassTag[_], Logging[F]])
        )(new CachedLogs[I, F](logs, _, _))

    def universal(implicit il: Lift[I, F], F: FlatMap[F]): Universal[F] = new UniversalEmbedLogs(logs)

    def cachedUniversal(
        implicit IM: Monad[I],
        IQ: QVars[I],
        IG: Guarantee[I],
        il: Lift[I, F],
        F: FlatMap[F]
    ): I[Universal[F]] = cached.map(_.universal)
  }
}

private[logging] trait LogsInstances0 extends LogsInstances1 {
  implicit def logs2ApplyK[Y[_]](implicit Y: Apply[Y]): ApplyK[Logs[Y, *[_]]] =
    new Logs2ApplyK[Y] { def I: Apply[Y] = Y }
}

private[logging] trait LogsInstances1 {
  implicit def logs2FunctorK[Y[_]](implicit Y: Functor[Y]): FunctorK[Logs[Y, *[_]]] =
    new Logs2FunctorK[Y] { def I: Functor[Y] = Y }
}

trait Logs2FunctorK[Y[_]] extends FunctorK[Logs[Y, *[_]]] {
  implicit def I: Functor[Y]

  def mapK[F[_], G[_]](af: Logs[Y, F])(fk: F ~> G): Logs[Y, G] = new Logs[Y, G] {
    def forService[Svc: ClassTag]: Y[Logging[G]] = af.forService[Svc].map(_.mapK(fk))
    def byName(name: String): Y[Logging[G]]      = af.byName(name).map(_.mapK(fk))
  }
}

trait Logs2ApplyK[Y[_]] extends Logs2FunctorK[Y] with ApplyK[Logs[Y, *[_]]] {
  implicit def I: Apply[Y]

  def zipWith2K[F[_], G[_], H[_]](af: Logs[Y, F], ag: Logs[Y, G])(f2: Function2K[F, G, H]): Logs[Y, H] =
    new Logs[Y, H] {
      def forService[Svc: ClassTag]: Y[Logging[H]] = (af.forService[Svc], ag.forService[Svc]).mapN(_.zipWithK(_)(f2))
      def byName(name: String): Y[Logging[H]]      = (af.byName(name), ag.byName(name)).mapN(_.zipWithK(_)(f2))
    }

  def productK[F[_], G[_]](af: Logs[Y, F], ag: Logs[Y, G]): Logs[Y, Tuple2K[F, G, *]] =
    zipWith2K(af, ag)(Function2K((f, g) => Tuple2K(f, g)))
}

trait Logs2MonoidalK[Y[_]] extends Logs2ApplyK[Y] with MonoidalK[Logs[Y, *[_]]] {
  implicit def I: Applicative[Y]

  def pureK[F[_]](p: Point[F]): Logs[Y, F] = new Logs[Y, F] {
    def forService[Svc: ClassTag]: Y[Logging[F]] = p.pureK[Logging].pure[Y]
    def byName(name: String): Y[Logging[F]]      = p.pureK[Logging].pure[Y]
  }
}
