package tofu
package fs2Instances
import cats.{FlatMap, Functor, ~>}
import cats.tagless.FunctorK
import tofu.higherKind.Embed
import tofu.syntax.funk._
import fs2._

private[fs2Instances] trait Fs2Instances1 extends Fs2Instances2 {
  private[this] val fs2HKInstanceAny = new FS2StreamHKInstance[Any]

  final implicit def fs2StreamHKInstance[A]: FS2StreamHKInstance[A] =
    fs2HKInstanceAny.asInstanceOf[FS2StreamHKInstance[A]]

  final implicit def fs2StreamRunContext[F[_], G[_], R](
      implicit fctx: HasContextRun[F, G, R]
  ): WithRun[Stream[F, *], Stream[G, *], R] =
    new FS2RunContext[F, G, R] { override val F: HasContextRun[F, G, R] = fctx }
}

private[fs2Instances] trait Fs2Instances2 extends Fs2Instances3 {
  final implicit def fs2StreamLocal[F[_], R](implicit fctx: F HasLocal R): WithLocal[Stream[F, *], R] =
    new FS2Local[F, R] { override val F: F HasLocal R = fctx }

  final implicit def fs2StreamProvide[F[_], G[_], R](
      implicit fctx: HasProvide[F, G, R]
  ): WithProvide[Stream[F, *], Stream[G, *], R] =
    new FS2Provide[F, G, R] { override val F: HasProvide[F, G, R] = fctx }
}

private[fs2Instances] trait Fs2Instances3 {
  final implicit def fs2StreamContext[F[_], R](implicit fctx: F HasContext R): WithContext[Stream[F, *], R] =
    new FS2Context[F, R] { override val F: F HasContext R = fctx }
}

class FS2StreamHKInstance[A] extends Embed[Stream[*[_], A]] with FunctorK[Stream[*[_], A]] {
  def embed[F[_]: FlatMap](ft: F[Stream[F, A]]): Stream[F, A]      = fs2.Stream.force(ft)
  def mapK[F[_], G[_]](af: Stream[F, A])(fk: F ~> G): Stream[G, A] = af.translate(fk)
}

trait FS2Context[F[_], R] extends WithContext[Stream[F, *], R] {
  implicit def F: F HasContext R

  val functor: Functor[Stream[F, *]] = implicitly
  def context: Stream[F, R]          = Stream.eval(F.context)
}

trait FS2Local[F[_], R] extends FS2Context[F, R] with WithLocal[Stream[F, *], R] {
  implicit def F: F HasLocal R

  def local[A](fa: Stream[F, A])(project: R => R): Stream[F, A] = fa.translate(funKFrom[F](F.local(_)(project)))
}

trait FS2Provide[F[_], G[_], R] extends WithProvide[Stream[F, *], Stream[G, *], R] {
  implicit def F: HasProvide[F, G, R]

  def runContext[A](fa: Stream[F, A])(ctx: R): Stream[G, A] = fa.translate(funKFrom[F](F.runContext(_)(ctx)))
  def lift[A](fa: Stream[G, A]): Stream[F, A]               = fa.translate(funKFrom[G](F.lift(_)))
}

trait FS2RunContext[F[_], G[_], R]
    extends FS2Local[F, R] with FS2Provide[F, G, R] with WithRun[Stream[F, *], Stream[G, *], R] {
  implicit def F: HasContextRun[F, G, R]
}
