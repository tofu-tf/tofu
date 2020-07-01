package tofu
package fs2Instances
import cats.effect.Concurrent
import cats.tagless.FunctorK
import cats.{Alternative, FlatMap, Functor, Monad, ~>}
import fs2._
import tofu.higherKind.Embed
import tofu.streams.{Chunks, CombineK, Evals, Merge}
import tofu.syntax.funk._

private[fs2Instances] trait Fs2Instances1 extends Fs2Instances2 {
  private[this] val fs2HKInstanceAny = new FS2StreamHKInstance[Any]

  final implicit def fs2StreamHKInstance[A]: FS2StreamHKInstance[A] =
    fs2HKInstanceAny.asInstanceOf[FS2StreamHKInstance[A]]

  final implicit def fs2StreamRunContext[F[_], G[_], R](implicit
      fctx: HasContextRun[F, G, R]
  ): WithRun[Stream[F, *], Stream[G, *], R]                   =
    new FS2RunContext[F, G, R] { override val F: HasContextRun[F, G, R] = fctx }

  implicit def fs2EvalsInstance[F[_]]: Evals[Stream[F, *], F] =
    new Evals[Stream[F, *], F] {
      override val monad: Monad[Stream[F, *]]             = Stream.monadInstance
      override val alternative: Alternative[Stream[F, *]] = fs2AlternativeInstance
      override def eval[A](ga: F[A]): Stream[F, A]        = Stream.eval(ga)
    }

  implicit def fs2ChunksInstance[F[_]]: Chunks[Stream[F, *], Chunk] =
    new Chunks[Stream[F, *], Chunk] {
      override def chunkN[A](fa: Stream[F, A])(n: Int): Stream[F, Chunk[A]]                 = fa.chunkN(n)
      override def chunks[A](fa: Stream[F, A]): Stream[F, Chunk[A]]                         = fa.chunks
      override def mapChunks[A, B](fa: Stream[F, A])(f: Chunk[A] => Chunk[B]): Stream[F, B] = fa.mapChunks(f)
    }

  implicit def fs2MergeInstance[F[_]: Concurrent]: Merge[Stream[F, *]] =
    new Merge[Stream[F, *]] {
      override def merge[A](fa: Stream[F, A])(that: Stream[F, A]): Stream[F, A] = fa merge that
    }

  implicit def fs2CombineKInstance[F[_]]: CombineK[Stream[F, *]] =
    new CombineK[Stream[F, *]] {
      override def combineK_[A](a: Stream[F, A])(b: => Stream[F, A]): Stream[F, A] = a ++ b
    }
}

private[fs2Instances] trait Fs2Instances2 extends Fs2Instances3 {
  final implicit def fs2StreamLocal[F[_], R](implicit fctx: F HasLocal R): WithLocal[Stream[F, *], R] =
    new FS2Local[F, R] { override val F: F HasLocal R = fctx }

  final implicit def fs2StreamProvide[F[_], G[_], R](implicit
      fctx: HasProvide[F, G, R]
  ): WithProvide[Stream[F, *], Stream[G, *], R]                                                       =
    new FS2Provide[F, G, R] { override val F: HasProvide[F, G, R] = fctx }
}

private[fs2Instances] trait Fs2Instances3 extends Fs2Instances4 {
  final implicit def fs2StreamContext[F[_], R](implicit fctx: F HasContext R): WithContext[Stream[F, *], R] =
    new FS2Context[F, R] { override val F: F HasContext R = fctx }
}

private[fs2Instances] trait Fs2Instances4 {
  implicit def fs2AlternativeInstance[F[_]]: Alternative[Stream[F, *]] =
    new Alternative[Stream[F, *]] {
      override def ap[A, B](ff: Stream[F, A => B])(fa: Stream[F, A]): Stream[F, B] = ff.flatMap(fa.map)
      override def empty[A]: Stream[F, A]                                          = Stream.empty
      override def combineK[A](x: Stream[F, A], y: Stream[F, A]): Stream[F, A]     = x ++ y
      override def pure[A](x: A): Stream[F, A]                                     = Stream.emit(x)
    }
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
