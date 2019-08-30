package tofu
package config

import cats.{Monad, MonadError, Parallel}
import cats.data.{Chain, Kleisli, NonEmptyList, ReaderT}
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.effect.Sync
import cats.effect.concurrent.Ref
import tofu.config.ConfigErrors.Message
import tofu.config.ConfigItem.ValueTag
import tofu.optics.{Contains, PContains}
import cats.syntax.option._
import tofu.config.ConfigTContext.configTRestore

trait ConfigErrors[F[_]] extends ConfigErrors.NotFound[F] with ConfigErrors.Variants[F] with ConfigErrors.Value[F]

object ConfigErrors {
  def notFound[F[_], A](implicit nf: NotFound[F]): F[A] = nf.notFound
  def badType[F[_], A](expected: ValueTag*)(actual: ValueTag)(implicit error: Value[F]): F[A] =
    error.badType(expected.toList, actual)

  trait NotFound[F[_]] {
    def notFound[A]: F[A]
  }

  trait Variants[F[_]] {
    def multipleVariants[A](variants: NonEmptyList[String]): F[A]
    def noVariantFound[A]: F[A]
  }

  trait Value[F[_]] {
    def badType[A](expected: List[ValueTag], actual: ValueTag): F[A]
    def badString[A](value: String, message: String): F[A]
    def badNumber[A](value: BigDecimal, message: String): F[A]
    def invalid[A](message: String): F[A]
  }

  sealed trait Message
  object Message {
    case object NotFound                                                 extends Message
    case object NoVariantFound                                           extends Message
    final case class BadType(expected: List[ValueTag], actual: ValueTag) extends Message
    final case class BadNumber(value: BigDecimal, message: String)       extends Message
    final case class BadString(value: String, message: String)           extends Message
    final case class Invalid(message: String)                            extends Message
    final case class MultipleVariants(variants: NonEmptyList[String])    extends Message
  }

  def byError[F[_]: Monad](implicit error: F Raise Message): ConfigErrors[F] =
    new ConfigErrors[F] {
      def notFound[A]: F[A]                                            = error.raise(Message.NotFound)
      def badType[A](expected: List[ValueTag], actual: ValueTag): F[A] = error.raise(Message.BadType(expected, actual))
      def badString[A](value: String, message: String): F[A]           = error.raise(Message.BadString(value, message))
      def badNumber[A](value: BigDecimal, message: String): F[A]       = error.raise(Message.BadNumber(value, message))
      def invalid[A](message: String): F[A]                            = error.raise(Message.Invalid(message))
      def multipleVariants[A](variants: NonEmptyList[String]): F[A]    = error.raise(Message.MultipleVariants(variants))
      def noVariantFound[A]: F[A]                                      = error.raise(Message.NoVariantFound)
    }
}

trait ConfigMonad[F[+_]] {
  implicit def monad: Monad[F]
  implicit def paralleled: Parallel[F]
  implicit def config: ConfigErrors[F]
  implicit def restore: Restore[F]
  implicit def path: HasLocal[F, Path]
}

object ConfigMonad {
  def apply[F[+_]](implicit F: ConfigMonad[F]): ConfigMonad[F] = F

  def tryParseSync[F[+_]: Sync: ParallelReader, A: Configurable](
      item: ConfigItem[F]
  ): F[(Vector[(Path, Message)], Option[A])] = {
    implicit val configTMonad: Monad[ConfigT[F, *]] = ReaderT.catsDataMonadForKleisli[F, ConfigTContext[F]]
    for {
      ref      <- Ref[F].of(Vector[(Path, Message)]())
      reader   = Configurable.parse[ConfigT[F, +*], A](item.mapK[ConfigT[F, +*]](ReaderT.liftK[F, ConfigTContext[F]]))
      context  = ConfigTContext[F](Vector.empty, ref)
      res      <- configTRestore[F].restore(reader.run(context))
      messages <- ref.get
    } yield (messages, res)
  }

  def fromMonadAndErrors[F[+_]](
      implicit
      F: Monad[F],
      FE: ConfigErrors[F],
      FR: Restore[F],
      FL: HasLocal[F, Path]
  ): ConfigMonad[F] =
    new ConfigMonad[F] {
      val monad      = F
      val config     = FE
      val paralleled = Parallel.identity[F]
      val restore    = FR
      val path       = FL
    }

  def fromParalleledAndErrors[F[+_]](
      implicit
      FP: Parallel[F],
      FE: ConfigErrors[F],
      F: Monad[F],
      FR: Restore[F],
      FL: HasLocal[F, Path]
  ): ConfigMonad[F] =
    new ConfigMonad[F] {
      val monad      = F
      val paralleled = FP
      val config     = FE
      val restore    = FR
      val path       = FL
    }

  object promote {
    implicit def monadByConfig[F[+_]](implicit F: ConfigMonad[F]): Monad[F]         = F.monad
    implicit def errorsByConfig[F[+_]](implicit F: ConfigMonad[F]): ConfigErrors[F] = F.config
    implicit def paralleledByConfig[F[+_]](implicit F: ConfigMonad[F]): Parallel[F] = F.paralleled
    implicit def restoredByConfig[F[+_]](implicit F: ConfigMonad[F]): Restore[F]    = F.restore
  }
}

final case class ConfigTContext[F[_]](path: Path, ref: Ref[F, Vector[(Path, Message)]])

object ConfigTContext {
  implicit def configTParallelConfigMonad[F[+_]: Sync: ParallelReader]: ConfigMonad[ConfigT[F, +*]] =
    new ConfigTConfigMonad[F]

  def contextPath[F[_]]: ConfigTContext[F] Contains Path =
    new Contains[ConfigTContext[F], Path] {
      def set(s: ConfigTContext[F], b: Path): ConfigTContext[F] = s.copy(path = b)
      def extract(s: ConfigTContext[F]): Path                   = s.path
    }

  def configTRestore[F[+_]](implicit F: Sync[F]): Restore[F] = new Restore[F] {
    def restoreWith[A](fa: F[A])(ra: => F[A]): F[A] = F.recoverWith(fa) { case Fail         => ra }
    def restore[A](fa: F[A]): F[Option[A]]          = F.recover(fa.map(_.some)) { case Fail => none }
  }

  class ConfigTConfigMonad[F[+_]](implicit F: Sync[F], FP: ParallelReader[F]) extends ConfigMonad[ConfigT[F, +*]] {

    val monad: MonadError[ConfigT[F, *], Throwable] = MonadError[ConfigT[F, *], Throwable]
    val paralleled: Parallel[ConfigT[F, *]]         = FP.paralleled
    val path: HasLocal[ConfigT[F, *], Path]         = Local[ConfigT[F, *]].subcontext(contextPath[F])
    val restore: Restore[ConfigT[F, +*]]            = configTRestore[ConfigT[F, +*]]
    val config: ConfigErrors[ConfigT[F, *]] = {
      implicit val error: ConfigT[F, *] Raise Message =
        new (ConfigT[F, *] Raise Message) {
          def raise[A](err: Message): ConfigT[F, A] =
            for {
              ctx <- ReaderT.ask[F, ConfigTContext[F]]
              _   <- ReaderT.liftF(ctx.ref.update(_ :+ (ctx.path -> err)))
              res <- monad.raiseError[A](Fail)
            } yield res
        }

      ConfigErrors.byError
    }
  }

  case object Fail extends Exception
}

final case class ParallelReader[F[+_]](paralleled: Parallel[ConfigT[F, *]]) extends AnyVal

object ParallelReader extends LowPriorityParallelReader {
  implicit def findParallel[F[+_]](implicit para: Parallel[F]) =
    ParallelReader[F](Kleisli.catsDataParallelForKleisli[F, ConfigTContext[F]])
}
trait LowPriorityParallelReader {
  implicit def provideIdentity[F[+_]](implicit F: Monad[F]) =
    ParallelReader[F](Parallel.identity[ConfigT[F, *]](Kleisli.catsDataMonadForKleisli[F, ConfigTContext[F]]))
}
