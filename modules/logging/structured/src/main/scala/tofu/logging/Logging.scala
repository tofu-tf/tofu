package tofu.logging

import cats.kernel.Monoid
import cats.{Applicative, Apply, FlatMap, Id}
import org.slf4j.Marker
import tofu.compat.unused
import tofu.higherKind.Function2K
import tofu.logging.Logging.{Debug, Error, Info, Level, Trace, Warn}
import tofu.logging.impl.{EmbedLogging, UniversalContextLogs, UniversalLogging}
import tofu.syntax.monadic._
import tofu.syntax.monoidalK._
import tofu.{Delay, Init, WithContext}

import scala.reflect.ClassTag

/** Typeclass equivalent of Logger. May contain specified some Logger instance or try to read it from the context
  */
@deprecated("Use Logging[F] instead", since = "0.10.4")
trait LoggingBase[F[_]] {

  /** push new message to log, level will be automatically checked
    * @param level
    *   desired level of logging, message will not be rendered\sent, if logging level of current logger is not low
    *   enough
    * @param message
    *   composed string with `{}` placeholders for values do not create strings on each call, use constant template
    *   string instead
    * @param values
    *   log parameters , values of types having `Loggable` instance would be converted automatically
    */
  def write(level: Level, message: String, values: LoggedValue*): F[Unit]

  /** could be overridden in the implementation, same as `write` but add additional info via marker */
  def writeMarker(level: Level, message: String, @unused marker: Marker, values: LoggedValue*): F[Unit] =
    write(level, message, values: _*)

  /** could be overridden in the implementations, write message about some exception */
  def writeCause(level: Level, message: String, cause: Throwable, values: LoggedValue*): F[Unit] =
    write(level, message, values :+ LoggedValue.error(cause): _*)

  def trace(message: String, values: LoggedValue*): F[Unit] = write(Trace, message, values: _*)
  def debug(message: String, values: LoggedValue*): F[Unit] = write(Debug, message, values: _*)
  def info(message: String, values: LoggedValue*): F[Unit]  = write(Info, message, values: _*)
  def warn(message: String, values: LoggedValue*): F[Unit]  = write(Warn, message, values: _*)
  def error(message: String, values: LoggedValue*): F[Unit] = write(Error, message, values: _*)

  def traceWithMarker(message: String, marker: Marker, values: LoggedValue*): F[Unit] =
    writeMarker(Trace, message, marker, values: _*)
  def debugWithMarker(message: String, marker: Marker, values: LoggedValue*): F[Unit] =
    writeMarker(Debug, message, marker, values: _*)
  def infoWithMarker(message: String, marker: Marker, values: LoggedValue*): F[Unit]  =
    writeMarker(Info, message, marker, values: _*)
  def warnWithMarker(message: String, marker: Marker, values: LoggedValue*): F[Unit]  =
    writeMarker(Warn, message, marker, values: _*)
  def errorWithMarker(message: String, marker: Marker, values: LoggedValue*): F[Unit] =
    writeMarker(Error, message, marker, values: _*)

  def traceCause(message: String, cause: Throwable, values: LoggedValue*): F[Unit] =
    writeCause(Trace, message, cause, values: _*)
  def debugCause(message: String, cause: Throwable, values: LoggedValue*): F[Unit] =
    writeCause(Debug, message, cause, values: _*)
  def infoCause(message: String, cause: Throwable, values: LoggedValue*): F[Unit]  =
    writeCause(Info, message, cause, values: _*)
  def warnCause(message: String, cause: Throwable, values: LoggedValue*): F[Unit]  =
    writeCause(Warn, message, cause, values: _*)
  def errorCause(message: String, cause: Throwable, values: LoggedValue*): F[Unit] =
    writeCause(Error, message, cause, values: _*)

  def asLogging: Logging[F]
}

/** Logging tagged with some arbitrary tag type.
  *
  * @note
  *   there are no guarantees that `Service` correspond to the type parameter of `Logs.forService` method
  */
trait ServiceLogging[F[_], Service] extends LoggingBase[F] {
  final def to[Svc2]: ServiceLogging[F, Svc2] = this.asInstanceOf[ServiceLogging[F, Svc2]]
}

object ServiceLogging extends ServiceLoggingRepresentableKInstances {
  implicit def initByLogs[I[_], F[_], Svc: ClassTag](implicit logs: Logs[I, F]): Init[I, ServiceLogging[F, Svc]] =
    new Init[I, ServiceLogging[F, Svc]] {
      def init: I[ServiceLogging[F, Svc]] = logs.service[Svc]
    }

  final implicit def byUniversal[F[_], Svc: ClassTag](implicit unilogs: Logging.Make[F]): ServiceLogging[F, Svc] =
    unilogs.service[Svc]
}

/** Typeclass for logging.
  * @see
  *   [[tofu.logging.Logs]] for creating instances of that trait
  */
trait Logging[F[_]] extends ServiceLogging[F, Nothing] {
  final def widen[G[a] >: F[a]]: Logging[G] = this.asInstanceOf[Logging[G]]

  def asLogging: Logging[F] = this
}

object Logging extends LoggingRepresentableKInstances {

  type Make[F[_]] = Logs[Id, F]

  object Make {
    def apply[F[_]: Logging.Make]: Logging.Make[F] = implicitly

    def plain[F[_]: Delay]: Logging.Make[F] = new UniversalLogging[F](_)

    def contextual[F[_]: FlatMap: Delay, C: Loggable](implicit FC: F WithContext C): Logging.Make[F] =
      new UniversalContextLogs[F, C]

  }

  /** Convenient alias type for LoggingCompanion for easier access. */
  type Companion[U[_[_]]] = LoggingCompanion[U]

  sealed trait Level

  case object Trace extends Level
  case object Debug extends Level
  case object Info  extends Level
  case object Warn  extends Level
  case object Error extends Level

  def mid: LoggingMidFunctions.type = LoggingMidFunctions

  type ForService[F[_], Svc] <: Logging[F]

  type Safe[F[_, _]]     = Logging[F[Nothing, _]]
  type SafeBase[F[_, _]] = LoggingBase[F[Nothing, _]]

  def apply[F[_]](implicit logging: Logging[F]): Logging[F] = logging

  /** the do-nothing Logging */
  def empty[F[_]: Applicative]: Logging[F] = new EmptyLogging[F]

  /** having two logging implementation call `first` after `second` */
  def combine[F[_]: Apply](first: Logging[F], second: Logging[F]): Logging[F] =
    first.zipWithK[F, F](second)(Function2K[F, F, F](_ *> _))

  def flatten[F[_]: FlatMap](underlying: F[Logging[F]]): Logging[F] = new EmbedLogging[F](underlying)

  implicit def loggingMonoid[F[_]: Applicative]: Monoid[Logging[F]] = new Monoid[Logging[F]] {
    val empty: Logging[F]                                 = Logging.empty[F]
    def combine(x: Logging[F], y: Logging[F]): Logging[F] = Logging.combine(x, y)
  }

}

private[tofu] class EmptyLogging[F[_]: Applicative] extends Logging[F] {
  private[this] val noop                                                  = Applicative[F].unit
  def write(level: Level, message: String, values: LoggedValue*): F[Unit] = noop
}
