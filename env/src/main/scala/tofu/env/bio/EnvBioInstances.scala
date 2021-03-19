package tofu.env.bio

import tofu.control.Bind
import tofu.control.StackSafeBind
import tofu.bi.BiRun
import tofu.higherKind.bi.FunBK

trait EnvBioInstances {}

class EnvBioBifunctorInstance[R]
    extends StackSafeBind[EnvBio[R, *, *]] with BiRun[EnvBio[R, *, *], BiTask, Nothing, R] {

  override def disclose[E, A](k: FunBK[EnvBio[R, *, *], BiTask] => EnvBio[R, E, A]): EnvBio[R, E, A] =
    EnvBio.context.flatMap((ctx: R) => k(FunBK.apply(bio => bio.run(ctx))))

  override def bifunctor: Bind[EnvBio[R, *, *]] = this

  override def lift[E, A](fa: BiTask[E, A]): EnvBio[R, E, A] = EnvBio.fromTaskEither(fa)

  override def runLeft[E, A](fa: EnvBio[R, E, A])(x: Nothing): BiTask[E, A] = x

  override def runRight[E, A](fa: EnvBio[R, E, A])(c: R): BiTask[E, A] = fa.run(c)

  override def context: EnvBio[R, Nothing, R] = EnvBio.context[R]

  override def bilocal[E, A](fea: EnvBio[R, E, A])(lproj: Nothing => Nothing, rproj: R => R): EnvBio[R, E, A] =
    fea.local(rproj)

  override def pure[E, A](a: A): EnvBio[R, E, A] = EnvBio.pure(a)

  override def raise[E, A](e: E): EnvBio[R, E, A] = EnvBio.raiseError(e)

  override def foldWith[E, A, X, B](
      fa: EnvBio[R, E, A],
      h: E => EnvBio[R, X, B],
      f: A => EnvBio[R, X, B]
  ): EnvBio[R, X, B] = fa.foldWith(h, f)

  override def flatMap[E, A, B](fa: EnvBio[R, E, A], f: A => EnvBio[R, E, B]): EnvBio[R, E, B] =
    fa.flatMap(f)

  override def map[E, A, B](fa: EnvBio[R, E, A])(f: A => B): EnvBio[R, E, B] = fa.map(f)

  override def mapErr[E, A, X](fa: EnvBio[R, E, A])(f: E => X): EnvBio[R, X, A] = fa.mapError(f)

  override def handleWith[E, X, A](fa: EnvBio[R, E, A], h: E => EnvBio[R, X, A]): EnvBio[R, X, A] =
    fa.onErrorHandleWith(h)
}
