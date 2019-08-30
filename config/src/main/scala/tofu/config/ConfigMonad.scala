package tofu
package config

import cats.data.ReaderT
import cats.effect.concurrent.Ref
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.{Monad, Parallel}
import tofu.concurrent._
import tofu.config.ConfigErrors.Message
import tofu.config.ConfigTContext.Fail
import tofu.optics.Contains

trait ConfigMonad[F[_]] {
  implicit def monad: Monad[F]
  implicit def paralleled: Parallel[F]
  implicit def config: ConfigErrors[F]
  implicit def restore: Restore[F]
  implicit def path: HasLocal[F, Path]
}

object ConfigMonad {
  def apply[F[_]](implicit F: ConfigMonad[F]): ConfigMonad[F] = F

  def tryParse[F[_]: Monad: Refs: ParallelReader, A: Configurable](
      item: ConfigItem[F]
  )(implicit errors: Errors[F, Fail.type]): F[(Vector[(Path, Message)], Option[A])] = {
    for {
      ref      <- newRef[F].of(Vector[(Path, Message)]())
      reader   = Configurable.parse[ConfigT[F, +*], A](item.mapK[ConfigT[F, +*]](ReaderT.liftK[F, ConfigTContext[F]]))
      context  = ConfigTContext[F](Vector.empty, ref)
      res      <- errors.restore(reader.run(context))
      messages <- ref.get
    } yield (messages, res)
  }

  def fromMonadAndErrors[F[_]](
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

  def fromParallelAndErrors[F[_]](
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
    implicit def monadByConfig[F[_]](implicit F: ConfigMonad[F]): Monad[F]          = F.monad
    implicit def errorsByConfig[F[_]](implicit F: ConfigMonad[F]): ConfigErrors[F]  = F.config
    implicit def paralleledByConfig[F[_]](implicit F: ConfigMonad[F]): Parallel[F]  = F.paralleled
    implicit def restoredByConfig[F[_]](implicit F: ConfigMonad[F]): Restore[F]     = F.restore
    implicit def localByConfig[F[_]](implicit F: ConfigMonad[F]): HasLocal[F, Path] = F.path
  }
}

final case class ConfigTContext[F[_]](path: Path, ref: Ref[F, Vector[(Path, Message)]])

object ConfigTContext {
  implicit def configTParallelConfigMonad[F[_]: ParallelReader](
      implicit F: Monad[F],
      FE: Errors[F, Fail.type]
  ): ConfigMonad[ConfigT[F, +*]] =
    new ConfigTConfigMonad[F]

  def contextPath[F[_]]: ConfigTContext[F] Contains Path =
    new Contains[ConfigTContext[F], Path] {
      def set(s: ConfigTContext[F], b: Path): ConfigTContext[F] = s.copy(path = b)
      def extract(s: ConfigTContext[F]): Path                   = s.path
    }

  class ConfigTConfigMonad[F[_]](implicit F: Monad[F], FP: ParallelReader[F], FR: Errors[F, Fail.type])
      extends ConfigMonad[ConfigT[F, *]] {

    val monad: Monad[ConfigT[F, *]]               = Monad[ConfigT[F, *]]
    val paralleled: Parallel[ConfigT[F, *]]       = FP.paralleled
    val path: HasLocal[ConfigT[F, *], Path]       = Local[ConfigT[F, *]].subcontext(contextPath[F])
    val restore: Errors[ConfigT[F, *], Fail.type] = Errors[ConfigT[F, *], Fail.type]
    val config: ConfigErrors[ConfigT[F, *]] = {
      implicit val error: ConfigT[F, *] Raise Message =
        new (ConfigT[F, *] Raise Message) {
          def raise[A](err: Message): ConfigT[F, A] =
            for {
              ctx <- ReaderT.ask[F, ConfigTContext[F]]
              _   <- ReaderT.liftF(ctx.ref.update(_ :+ (ctx.path -> err)))
              res <- restore.raise[A](Fail)
            } yield res
        }

      ConfigErrors.byError
    }
  }

  case object Fail extends Exception
}

final case class ParallelReader[F[_]](paralleled: Parallel[ConfigT[F, *]]) extends AnyVal

object ParallelReader extends LowPriorityParallelReader {
  implicit def findParallel[F[_]](implicit para: Parallel[F]) = ParallelReader[F](implicitly)
}
trait LowPriorityParallelReader {
  implicit def provideIdentity[F[_]](implicit F: Monad[F]) = ParallelReader[F](Parallel.identity[ConfigT[F, *]])
}
