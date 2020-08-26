package tofu
package fs2Instances
import cats.effect.{Concurrent, ExitCase, Sync, Timer}
import cats.tagless.FunctorK
import cats.{FlatMap, Functor, Monad, MonoidK, ~>}
import fs2._
import tofu.higherKind.Embed
import tofu.streams.{Chunks, Compile, Evals, Merge, ParFlatten, RegionThrow, Temporal}
import tofu.syntax.funk._

import scala.concurrent.duration.FiniteDuration

private[fs2Instances] trait Fs2Instances1 extends Fs2Instances2 {
  private[this] val fs2HKInstanceAny = new FS2StreamHKInstance[Any]

  final implicit def fs2StreamHKInstance[A]: FS2StreamHKInstance[A] =
    fs2HKInstanceAny.asInstanceOf[FS2StreamHKInstance[A]]

  final implicit def fs2StreamRunContext[F[_], G[_], R](implicit
      fctx: HasContextRun[F, G, R]
  ): WithRun[Stream[F, *], Stream[G, *], R] =
    new FS2RunContext[F, G, R] {
      override val F: HasContextRun[F, G, R] = fctx
    }

  implicit def fs2EvalsInstance[F[_]]: Evals[Stream[F, *], F] =
    new Evals[Stream[F, *], F] {
      override val monad: Monad[Stream[F, *]]     = Stream.monadInstance
      override val monoidK: MonoidK[Stream[F, *]] = Stream.monoidKInstance

      override def eval[A](ga: F[A]): Stream[F, A] = Stream.eval(ga)
    }

  implicit def fs2ChunksInstance[F[_]]: Chunks[Stream[F, *], Chunk] =
    new Chunks[Stream[F, *], Chunk] {
      override def chunkN[A](fa: Stream[F, A])(n: Int): Stream[F, Chunk[A]] = fa.chunkN(n)

      override def chunks[A](fa: Stream[F, A]): Stream[F, Chunk[A]] = fa.chunks

      override def mapChunks[A, B](fa: Stream[F, A])(f: Chunk[A] => Chunk[B]): Stream[F, B] = fa.mapChunks(f)

      override def cons[A](fa: Stream[F, A])(c: Chunk[A]): Stream[F, A] = fa.cons(c)
    }

  implicit def fs2MergeInstance[F[_]: Concurrent]: Merge[Stream[F, *]] =
    new Merge[Stream[F, *]] {
      override def merge[A](fa: Stream[F, A])(that: Stream[F, A]): Stream[F, A] = fa merge that
    }

  implicit def fs2CompileInstance[F[_]: Sync]: Compile[Stream[F, *], F] =
    new Compile[Stream[F, *], F] {

      override def drain[A](fa: Stream[F, A]): F[Unit] = fa.compile.drain

      override def fold[A, B](fa: Stream[F, A])(init: B)(f: (B, A) => B): F[B] = fa.compile.fold(init)(f)

      override def to[C[_], A](fa: Stream[F, A])(implicit ev: tofu.streams.internal.Factory[A, C[A]]): F[C[A]] =
        fa.compile.to(ev)
    }

  implicit def fs2ParFlattenInstance[F[_]: Concurrent: Timer]: ParFlatten[Stream[F, *]] =
    new ParFlatten[Stream[F, *]] {
      override def parFlatten[A](ffa: Stream[F, Stream[F, A]])(maxConcurrent: Int): Stream[F, A] =
        ffa.parJoin(maxConcurrent)
    }

  implicit def fs2TemporalInstance[F[_]: Timer]: Temporal[Stream[F, *]] =
    new Temporal[Stream[F, *]] {

      override def metered[A](fa: Stream[F, A])(rate: FiniteDuration): Stream[F, A] = fa.metered(rate)

      override def delay[A](fa: Stream[F, A])(d: FiniteDuration): Stream[F, A] = fa.delayBy(d)
    }

  implicit def fs2RegionThrowInstance[F[_]]: RegionThrow[Stream[F, *], F] =
    new RegionThrow[Stream[F, *], F] {
      override def regionCase[R](open: F[R])(close: (R, ExitCase[Throwable]) => F[Unit]): Stream[F, R] =
        Stream.bracketCase(open)(close)
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
