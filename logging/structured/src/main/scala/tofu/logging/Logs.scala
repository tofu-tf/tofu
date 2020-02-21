package tofu.logging

import Logging.loggerForService
import cats.effect.Sync
import cats.kernel.Monoid
import cats.{Applicative, Apply, FlatMap, Functor}
import impl.{ContextSyncLoggingImpl, SyncLogging}
import org.slf4j.LoggerFactory
import tofu.higherKind
import tofu.higherKind.RepresentableK
import tofu.syntax.monadic._

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

  /** Creates an instance of [[tofu.logging.Logging]] with a given arbitrary type tag.
    * In a simple way that means creating a Logger for a given class. */
  def forService[Svc: ClassTag]: I[Logging[F]]

  /** Creates an instance of [[tofu.logging.Logging]] for a given artitrary string name.
    *  In a simple way that means creating a Logger with a given name.
    */
  def byName(name: String): I[Logging[F]]

  final def biwiden[I1[a] >: I[a], F1[a] >: F[a]]: Logs[I1, F1] = this.asInstanceOf[Logs[I1, F1]]

  final def service[Svc: ClassTag]: I[ServiceLogging[F, Svc]] = forService[Svc].asInstanceOf[I[ServiceLogging[F, Svc]]]
}

object Logs {
  def apply[I[_], F[_]](implicit logs: Logs[I, F]): Logs[I, F] = logs

  private[this] val logsRepresentableAny: RepresentableK[Logs[*[_], Any]] =
    higherKind.derived.genRepresentableK[Logs[*[_], Any]]

  implicit def logsRepresentable[Y[_]]: RepresentableK[Logs[*[_], Y]] =
    logsRepresentableAny.asInstanceOf[RepresentableK[Logs[*[_], Y]]]

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
    * Retuns an instance of [[tofu.logging.Logs]] that will produce a no-op [[tofu.logging.Logging]] instances.
    */
  def empty[I[_]: Applicative, F[_]: Applicative]: Logs[I, F] = const[I, F](Logging.empty[F])

  /**
    * Combines two instances of [[tofu.logging.Logs]], resulting in a single one that will produce
    * instances of [[tofu.logging.Logging]] by combining.
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
}
