package tofu.logging

import cats.{Applicative, Apply, FlatMap, Functor, Id, Monad}
import org.slf4j.LoggerFactory
import tofu.{Delay, WithContext}
import tofu.logging.impl.{ContextSyncLoggingImpl, SyncLogging, UniversalContextLogs, UniversalLogging}
import tofu.logging.internal.{LogsInstances, LogsInstances0, LogsInvariantSyntax}
import tofu.syntax.monadic._

import scala.reflect.ClassTag

/** A helper for creating instances of [[tofu.logging.Logging]], defining a way these instances will behave while doing
  * logging. Can create instances either on a by-name basic or a type tag basic. An instance of [[tofu.logging.Logs]]
  * can be shared between different pieces of code depending on whether logging behaviour should be shared. However it's
  * not uncommon to use different [[Logs]] for different parts of program.
  *
  * Sample usage would be:
  * @example
  *   {{{ val logs: Logs[F, F] = Logs.sync[F, F]
  *
  * def program[F[_]: Sync] = for { logging <- logs.byName("my-logger") _ <- logging.info("this is a message") _ <-
  * logging.info("this is another message") } yield () }}}
  */
trait Logs[+I[_], F[_]] extends LogsVOps[I, F] {

  /** Creates an instance of [[tofu.logging.Logging]] with a given arbitrary type tag, using it as a class to create
    * underlying [[org.slf4j.Logger]] with.
    */
  def forService[Svc](implicit Svc: ClassTag[Svc]): I[Logging[F]] = byName(Svc.runtimeClass.getName())

  /** Creates an instance of [[tofu.logging.Logging]] for a given arbitrary string name, using it to create underlying
    * [[org.slf4j.Logger]] with.
    */
  def byName(name: String): I[Logging[F]]

  final def biwiden[I1[a] >: I[a], F1[a] >: F[a]]: Logs[I1, F1] = this.asInstanceOf[Logs[I1, F1]]

  final def service[Svc: ClassTag]: I[ServiceLogging[F, Svc]] = forService[Svc].asInstanceOf[I[ServiceLogging[F, Svc]]]

  /** An alternative to [[service]] method for tagless final services.
    *
    * @example
    *   {{{ trait Service[F[_]]{ ...}
    *
    * object Service extends Logging.Companion[Service] val logs: Logs.Universal[F] = ???
    *
    * val serviceLog = logs.of[Service] //ServiceLogging[ }}}
    */
  final def of[Svc[_[_]]](implicit tag: ClassTag[Svc[Any]]): I[ServiceLogging[F, Svc[Any]]] =
    service[Svc[Any]]
}

object Logs extends LogsInstances with LogsInstances0 {

  @deprecated("Use Logging.Make[F]", since = "0.10.4")
  type Universal[F[_]]        = Logs[Id, F]
  type Safe[I[_], F[_, _]]    = Logs[I, F[Nothing, *]]
  type SafeUniversal[F[_, _]] = Logs[Id, F[Nothing, *]]

  def apply[I[_], F[_]](implicit logs: Logs[I, F]): Logs[I, F] = logs

  /** Returns an instance of [[tofu.logging.Logs]] that requires [[Delay]] to perform logging side-effects. Has no
    * notion of context.
    */
  def sync[I[_]: Delay, F[_]: Delay: Monad]: Logs[I, F] = new Logs[I, F] {
    def byName(name: String): I[Logging[F]] = Delay[I].delay(new SyncLogging[F](LoggerFactory.getLogger(name)))
  }
  @deprecated("Use Logging.Make.plain[F]", since = "0.10.4")
  def universal[F[_]: Delay]: Logs.Universal[F]         = new UniversalLogging[F](_)

  @deprecated("Use Logging.Make.contextual[F]", since = "0.10.4")
  def contextual[F[_]: FlatMap: Delay, C: Loggable](implicit FC: F WithContext C): Logs.Universal[F] =
    new UniversalContextLogs[F, C]

  def withContext[I[_]: Delay, F[_]: Monad: Delay](implicit ctx: LoggableContext[F]): Logs[I, F] = {

    import ctx.loggable
    new Logs[I, F] {
      override def forService[Svc](implicit ct: ClassTag[Svc]): I[Logging[F]] =
        Delay[I].delay(new ContextSyncLoggingImpl[F, ctx.Ctx](ctx.context, LoggerFactory.getLogger(ct.runtimeClass)))
      override def byName(name: String): I[Logging[F]]                        =
        Delay[I].delay(new ContextSyncLoggingImpl[F, ctx.Ctx](ctx.context, LoggerFactory.getLogger(name)))
    }
  }

  /** Returns an instance of [[tofu.logging.Logs]] that will produce the same constant [[tofu.logging.Logging]]
    * instances.
    */
  def const[I[_]: Applicative, F[_]](logging: Logging[F]): Logs[I, F] = new Logs[I, F] {
    def byName(name: String): I[Logging[F]] = logging.pure[I]
  }

  /** Returns an instance of [[tofu.logging.Logs]] that will produce a no-op [[tofu.logging.Logging]] instances.
    */
  def empty[I[_]: Applicative, F[_]: Applicative]: Logs[I, F] = const[I, F](Logging.empty[F])

  /** Combines two instances of [[tofu.logging.Logs]], resulting in a single one that will produce instances of
    * [[tofu.logging.Logging]] by combining. Resulting [[tofu.logging.Logging]] instance will call both implementations
    * in order.
    */
  def combine[I[_]: Apply, F[_]: Apply](las: Logs[I, F], lbs: Logs[I, F]): Logs[I, F] = new Logs[I, F] {
    def byName(name: String): I[Logging[F]] = las.byName(name).map2(lbs.byName(name))(Logging.combine[F])
  }

  /** Allows to create Logging instance for the service
    *
    * @example
    *   {{{ class MyService[F[_]](log: MyService.Log[F]) {...}
    *
    * object MyService extends LoggingCompanion[MyService] { def make[I[_], F[_]]: I[MyService[F]] =
    * Logs.provide[MyService[F]](new MyService[F](_)) } }}}
    */
  def provide[I[_], F[_]] = new ProvidePA[I, F]

  class ProvidePA[I[_], F[_]] {
    def apply[X: ClassTag](f: Logging[F] => X)(implicit logs: Logs[I, F], I: Functor[I]): I[X] =
      logs.forService[X].map(f)
  }

  /** Allows to create Logging instance for the service
    *
    * Same as [[provide]] but with ability to perform monadic actions.
    *
    * @example
    *   {{{ class MyService[F[_]](ref: Ref[F, Smth], log: MyService.Log[F]) {...}
    *
    * object MyService extends LoggingCompanion[MyService] { def make[I[_], F[_]]: I[MyService[F]] =
    * Logs.provide[MyService[F]]{ log => for { ref <- Ref.make(...) ... } yield new MyService(ref, log) } } }}}
    */
  def provideM[I[_], F[_]] = new ProvideMPA[I, F]

  class ProvideMPA[I[_], F[_]] {
    def apply[X: ClassTag](f: Logging[F] => I[X])(implicit logs: Logs[I, F], I: FlatMap[I]): I[X] =
      logs.forService[X].flatMap(f)
  }

  /** Set of useful syntax stuff for Logs */
  implicit def ops[I[_], F[_]](logs: Logs[I, F]): LogsInvariantSyntax.LogsOps[I, F] =
    new LogsInvariantSyntax.LogsOps[I, F](logs)
}
