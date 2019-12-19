package tofu.logging

import Logging._
import cats.kernel.Monoid
import cats.syntax.apply._
import cats.{Applicative, Apply, FlatMap}
import com.github.ghik.silencer.silent
import org.slf4j.{Logger, LoggerFactory, Marker}
import tofu.higherKind
import tofu.higherKind.{Embed, RepresentableK}
import tofu.logging.impl.EmbedLogging
import tofu.syntax.functionK._

import scala.reflect.ClassTag

/** typeclass equivalent of Logger
  * may contain specified some Logger instance
  * or try to read it from the context */
trait LoggingBase[F[_]] {

  /** push new message to log, level will be automatically checked
    * @param level desired level of logging, message will not be rendered\sent, if logging level
    *              of current logger is not low enough
    * @param message composed string with `{}` placeholders for values
    *                 do not create strings on each call, use constant template string instead
    * @param values  log parameters , values of types having `Loggable` instance would be converted automatically
    */
  def write(level: Level, message: String, values: LoggedValue*): F[Unit]

  /** could be overridden in the implementation, same as `write` but add additional info via marker */
  @silent def writeMarker(level: Level, message: String, marker: Marker, values: LoggedValue*): F[Unit] =
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
  def infoWithMarker(message: String, marker: Marker, values: LoggedValue*): F[Unit] =
    writeMarker(Info, message, marker, values: _*)
  def warnWithMarker(message: String, marker: Marker, values: LoggedValue*): F[Unit] =
    writeMarker(Warn, message, marker, values: _*)
  def errorWithMarker(message: String, marker: Marker, values: LoggedValue*): F[Unit] =
    writeMarker(Error, message, marker, values: _*)

  def traceCause(message: String, cause: Throwable, values: LoggedValue*): F[Unit] =
    writeCause(Trace, message, cause, values: _*)
  def debugCause(message: String, cause: Throwable, values: LoggedValue*): F[Unit] =
    writeCause(Debug, message, cause, values: _*)
  def infoCause(message: String, cause: Throwable, values: LoggedValue*): F[Unit] =
    writeCause(Info, message, cause, values: _*)
  def warnCause(message: String, cause: Throwable, values: LoggedValue*): F[Unit] =
    writeCause(Warn, message, cause, values: _*)
  def errorCause(message: String, cause: Throwable, values: LoggedValue*): F[Unit] =
    writeCause(Error, message, cause, values: _*)
}

/** Logging marked with Service type*/
trait ServiceLogging[F[_], +Service] extends LoggingBase[F]

object ServiceLogging{
  private [this] val representableAny: RepresentableK[ServiceLogging[*[_], Any]] =
    higherKind.derived.genRepresentableK[ServiceLogging[*[_], Any]]

  final implicit def serviceLoggingRepresentable[Svc] : RepresentableK[ServiceLogging[*[_], Any]] =
    representableAny.asInstanceOf[RepresentableK[ServiceLogging[*[_], Any]]]
}

/** typeclass for logging using specified logger or set of loggers
  * see `Logs` for creating instances of that*/
trait Logging[F[_]] extends ServiceLogging[F, Nothing] {
  final def widen[G[a] >: F[a]]: Logging[G] = this.asInstanceOf[Logging[G]]
}

object Logging {
  def apply[F[_]](implicit logging: Logging[F]): Logging[F] = logging

  /** the do-nothing Logging */
  def empty[F[_]: Applicative]: Logging[F] = new EmptyLogging[F]

  /** having two logging implementation call `first` after `second` */
  def combine[F[_]: Apply](first: Logging[F], second: Logging[F]): Logging[F] =
    loggingRepresentable.map2K(first, second)(makeFunctionK(t => t.first *> t.second))

  private[logging] def loggerForService[S](implicit ct: ClassTag[S]): Logger =
    LoggerFactory.getLogger(ct.runtimeClass)

  def flatten[F[_]: FlatMap](underlying: F[Logging[F]]): Logging[F] = new EmbedLogging[F](underlying)

  implicit val loggingRepresentable: RepresentableK[Logging] = higherKind.derived.genRepresentableK[Logging]

  implicit def loggingMonoid[F[_]: Applicative]: Monoid[Logging[F]] = new Monoid[Logging[F]] {
    val empty: Logging[F]                                 = Logging.empty[F]
    def combine(x: Logging[F], y: Logging[F]): Logging[F] = Logging.combine(x, y)
  }

  implicit val loggingEmbed: Embed[Logging] = new Embed[Logging] {
    def embed[F[_]: FlatMap](ft: F[Logging[F]]): Logging[F] = new EmbedLogging[F](ft)
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
  def write(level: Level, message: String, values: LoggedValue*): F[Unit] = noop
}
