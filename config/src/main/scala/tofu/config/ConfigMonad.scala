package tofu
package config

import cats.data.ReaderT
import cats.effect.concurrent.Ref
import cats.{Monad, Parallel}
import tofu.concurrent._
import tofu.config.ConfigTContext.Fail
import tofu.optics.Contains
import tofu.syntax.monadic._

trait ConfigMonad[F[_]] {
  implicit def monad: Monad[F]
  implicit def paralleled: Parallel[F]
  implicit def config: Raise[F, ConfigError]
  implicit def path: HasLocal[F, Path]
  implicit def restore: Restore[F]

  def error[A](err: ConfigError): F[A] = config.raise(err)
}

object ConfigMonad {
  def apply[F[_]](implicit F: ConfigMonad[F]): ConfigMonad[F] = F

  def tryParse[F[_]: Monad: Refs: ParallelReader, A: Configurable](
      item: ConfigItem[F]
  )(implicit errors: Errors[F, Fail.type]): F[(Vector[(Path, ConfigError)], Option[A])] = {
    for {
      ref      <- newRef[F].of(Vector[(Path, ConfigError)]())
      reader   = Configurable.parse[ConfigT[F, *], A](item.mapK[ConfigT[F, *]](ReaderT.liftK[F, ConfigTContext[F]]))
      context  = ConfigTContext[F](Vector.empty, ref)
      res      <- errors.restore(reader.run(context))
      messages <- ref.get
    } yield (messages, res)
  }

  def fromMonadAndErrors[F[_]](
      implicit
      F: Monad[F],
      FE: Errors[F, ConfigError],
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
      FE: Errors[F, ConfigError],
      F: Monad[F],
      FL: HasLocal[F, Path]
  ): ConfigMonad[F] =
    new ConfigMonad[F] {
      val monad      = F
      val paralleled = FP
      val config     = FE
      val path       = FL
      val restore    = FE
    }

  object promote {
    implicit def monadByConfig[F[_]](implicit F: ConfigMonad[F]): Monad[F]          = F.monad
    implicit def errorsByConfig[F[_]](implicit F: ConfigMonad[F]): ConfigRaise[F]   = F.config
    implicit def paralleledByConfig[F[_]](implicit F: ConfigMonad[F]): Parallel[F]  = F.paralleled
    implicit def localByConfig[F[_]](implicit F: ConfigMonad[F]): HasLocal[F, Path] = F.path
    implicit def restoreByConfig[F[_]](implicit F: ConfigMonad[F]): Restore[F]      = F.restore
  }
}

final case class ConfigTContext[F[_]](path: Path, ref: Ref[F, Vector[(Path, ConfigError)]])

object ConfigTContext {
  implicit def configTParallelConfigMonad[F[_]: ParallelReader](
      implicit F: Monad[F],
      FE: Errors[F, Fail.type]
  ): ConfigMonad[ConfigT[F, *]] = new ConfigTConfigMonad[F]

  def contextPath[F[_]]: ConfigTContext[F] Contains Path =
    new Contains[ConfigTContext[F], Path] {
      def set(s: ConfigTContext[F], b: Path): ConfigTContext[F] = s.copy(path = b)
      def extract(s: ConfigTContext[F]): Path                   = s.path
    }

  class ConfigTConfigMonad[F[_]: Monad](implicit FP: ParallelReader[F], FR: Errors[F, Fail.type])
      extends ConfigMonad[ConfigT[F, *]] {

    val monad: Monad[ConfigT[F, *]]         = Monad[ConfigT[F, *]]
    val paralleled: Parallel[ConfigT[F, *]] = FP.paralleled
    val path: HasLocal[ConfigT[F, *], Path] = Local[ConfigT[F, *]].subcontext(contextPath[F])
    val config: ConfigRaise[ConfigT[F, *]] = new ConfigRaise[ConfigT[F, *]] {
      def raise[A](err: ConfigError): ConfigT[F, A] = ReaderT(
        ctx => ctx.ref.update(_ :+ (ctx.path -> err)) *> FR.raise(Fail)
      )
    }
    val restore: Restore[ConfigT[F, *]] = new Restore[ConfigT[F, *]] {
      def restoreWith[A](fa: ConfigT[F, A])(ra: => ConfigT[F, A]): ConfigT[F, A] =
        ReaderT(ctx => FR.restoreWith(fa.run(ctx))(ra.run(ctx)))
      def restore[A](fa: ConfigT[F, A]): ConfigT[F, Option[A]] = ReaderT(ctx => FR.restore(fa.run(ctx)))
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
