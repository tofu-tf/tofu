package tofu.zioInstances

import tofu.bi.BiRun
import tofu.control.{Bind, StackSafeBind}
import zio.{IO, Tag, ZEnvironment, ZIO}

class ZioTofuBiInstance[R: Tag] extends StackSafeBind[ZIO[R, +_, +_]] with BiRun[ZIO[R, +_, +_], IO, R, R] {
  override def bifunctor: Bind[ZIO[R, +_, +_]] = this

  def pure[E, A](a: A): ZIO[R, E, A] = ZIO.succeed(a)

  def raise[E, A](e: E): ZIO[R, E, A] = ZIO.fail(e)

  def foldWith[E, A, X, B](fa: ZIO[R, E, A])(h: E => ZIO[R, X, B], f: A => ZIO[R, X, B]): ZIO[R, X, B] =
    fa.foldZIO(h, f)

  def context: ZIO[R, R, R] = ZIO.service

  def lift[E, A](fa: IO[E, A]): ZIO[R, E, A] = fa

  def runLeft[E, A](fa: ZIO[R, E, A])(x: R): IO[E, A] = fa.provideEnvironment(ZEnvironment(x))

  def runRight[E, A](fa: ZIO[R, E, A])(c: R): IO[E, A] = fa.provideEnvironment(ZEnvironment(c))
}
