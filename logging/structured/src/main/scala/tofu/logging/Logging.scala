package tofu.logging

import Logging._
import cats.kernel.Monoid
import cats.syntax.apply._
import cats.{Applicative, Apply, FlatMap}
import org.slf4j.{Logger, LoggerFactory, Marker}
import tofu.compat.unused
import tofu.{Init, higherKind}
import tofu.higherKind.{Function2K, RepresentableK}
import tofu.logging.impl.EmbedLogging
import tofu.logging.location.Location
import tofu.syntax.monoidalK._

import scala.reflect.ClassTag

/** typeclass equivalent of Logger
  * may contain specified some Logger instance
  * or try to read it from the context
  */
trait LoggingBase[F[_]] {

  /** Push new message to log, level will be automatically checked
    *
    * @param level desired level of logging, message will not be rendered\sent, if logging level
    *              of current logger is not low enough
    * @param message composed string with `{}` placeholders for values
    *                 do not create strings on each call, use constant template string instead
    * @param values  log parameters , values of types having `Loggable` instance would be converted automatically
    */
  def write(level: Level, location: Option[Location], message: String, values: LoggedValue*): F[Unit]

  /** could be overridden in the implementation, same as `write` but add additional info via marker */
  def writeMarker(
      level: Level,
      location: Option[Location],
      message: String,
      @unused marker: Marker,
      values: LoggedValue*
  ): F[Unit] =
    write(level, location, message, values: _*)

  /** could be overridden in the implementations, write message about some exception */
  def writeCause(
      level: Level,
      location: Option[Location],
      message: String,
      cause: Throwable,
      values: LoggedValue*
  ): F[Unit] =
    write(level, location, message, values :+ LoggedValue.error(cause): _*)

  def trace(message: String, location: Option[Location], values: LoggedValue*): F[Unit] =
    write(Trace, location, message, values: _*)
  def debug(message: String, location: Option[Location], values: LoggedValue*): F[Unit] =
    write(Debug, location, message, values: _*)
  def info(message: String, location: Option[Location], values: LoggedValue*): F[Unit]  =
    write(Info, location, message, values: _*)
  def warn(message: String, location: Option[Location], values: LoggedValue*): F[Unit]  =
    write(Warn, location, message, values: _*)
  def error(message: String, location: Option[Location], values: LoggedValue*): F[Unit] =
    write(Error, location, message, values: _*)

  def traceWithMarker(message: String, location: Option[Location], marker: Marker, values: LoggedValue*): F[Unit] =
    writeMarker(Trace, location, message, marker, values: _*)
  def debugWithMarker(message: String, location: Option[Location], marker: Marker, values: LoggedValue*): F[Unit] =
    writeMarker(Debug, location, message, marker, values: _*)
  def infoWithMarker(message: String, location: Option[Location], marker: Marker, values: LoggedValue*): F[Unit]  =
    writeMarker(Info, location, message, marker, values: _*)
  def warnWithMarker(message: String, location: Option[Location], marker: Marker, values: LoggedValue*): F[Unit]  =
    writeMarker(Warn, location, message, marker, values: _*)
  def errorWithMarker(message: String, location: Option[Location], marker: Marker, values: LoggedValue*): F[Unit] =
    writeMarker(Error, location, message, marker, values: _*)

  def traceCause(message: String, location: Option[Location], cause: Throwable, values: LoggedValue*): F[Unit] =
    writeCause(Trace, location, message, cause, values: _*)
  def debugCause(message: String, location: Option[Location], cause: Throwable, values: LoggedValue*): F[Unit] =
    writeCause(Debug, location, message, cause, values: _*)
  def infoCause(message: String, location: Option[Location], cause: Throwable, values: LoggedValue*): F[Unit]  =
    writeCause(Info, location, message, cause, values: _*)
  def warnCause(message: String, location: Option[Location], cause: Throwable, values: LoggedValue*): F[Unit]  =
    writeCause(Warn, location, message, cause, values: _*)
  def errorCause(message: String, location: Option[Location], cause: Throwable, values: LoggedValue*): F[Unit] =
    writeCause(Error, location, message, cause, values: _*)
}

/** Logging tagged with some arbitrary tag type
  *  note there are not any guarantees that `Service` correspond to the type parameter of `Logs.forService` method
  */
trait ServiceLogging[F[_], Service] extends LoggingBase[F] {
  final def to[Svc2]: ServiceLogging[F, Svc2] = this.asInstanceOf[ServiceLogging[F, Svc2]]
}

object ServiceLogging {
  private[this] val representableAny: RepresentableK[ServiceLogging[*[_], Any]] =
    higherKind.derived.genRepresentableK[ServiceLogging[*[_], Any]]

  implicit def initByLogs[I[_], F[_], Svc: ClassTag](implicit logs: Logs[I, F]): Init[I, ServiceLogging[F, Svc]] =
    new Init[I, ServiceLogging[F, Svc]] {
      def init: I[ServiceLogging[F, Svc]] = logs.service[Svc]
    }

  final implicit def serviceLoggingRepresentable[Svc]: RepresentableK[ServiceLogging[*[_], Svc]] =
    representableAny.asInstanceOf[RepresentableK[ServiceLogging[*[_], Svc]]]

  final implicit def byUniversal[F[_], Svc: ClassTag](implicit unilogs: Logs.Universal[F]): ServiceLogging[F, Svc] =
    unilogs.service[Svc]
}

/** typeclass for logging using specified logger or set of loggers
  * see `Logs` for creating instances of that
  */
trait Logging[F[_]] extends ServiceLogging[F, Nothing] {
  final def widen[G[a] >: F[a]]: Logging[G] = this.asInstanceOf[Logging[G]]
}

object Logging {
  type ForService[F[_], Svc] <: Logging[F]

  def apply[F[_]](implicit logging: Logging[F]): Logging[F] = logging

  /** the do-nothing Logging */
  def empty[F[_]: Applicative]: Logging[F] = new EmptyLogging[F]

  /** having two logging implementation call `first` after `second` */
  def combine[F[_]: Apply](first: Logging[F], second: Logging[F]): Logging[F] =
    first.zipWithK(second)(Function2K[F, F, F](_ *> _))

  private[logging] def loggerForService[S](implicit ct: ClassTag[S]): Logger =
    LoggerFactory.getLogger(ct.runtimeClass)

  def flatten[F[_]: FlatMap](underlying: F[Logging[F]]): Logging[F] = new EmbedLogging[F](underlying)

  implicit val loggingRepresentable: RepresentableK[Logging] = higherKind.derived.genRepresentableK[Logging]

  implicit def loggingMonoid[F[_]: Applicative]: Monoid[Logging[F]] = new Monoid[Logging[F]] {
    val empty: Logging[F]                                 = Logging.empty[F]
    def combine(x: Logging[F], y: Logging[F]): Logging[F] = Logging.combine(x, y)
  }

  /** log level enumeration */
  sealed trait Level

  case object Trace extends Level
  case object Debug extends Level
  case object Info  extends Level
  case object Warn  extends Level
  case object Error extends Level
}

private[tofu] class EmptyLogging[F[_]: Applicative] extends Logging[F] {
  private[this] val noop                                                  = Applicative[F].unit
  def write(level: Level, location: Option[Location], message: String, values: LoggedValue*): F[Unit] = noop
}

trait LoggingCompanion[U[_[_]]] {
  type Log[F[_]] = ServiceLogging[F, U[Any]]
}
