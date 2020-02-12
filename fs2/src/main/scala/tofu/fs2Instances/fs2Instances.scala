package tofu
package fs2Instances
import cats.{FlatMap, Functor, ~>}
import cats.tagless.FunctorK
import tofu.higherKind.Embed
import fs2._

class FS2StreamInstance[A] extends Embed[Stream[*[_], A]] with FunctorK[Stream[*[_], A]] {
  def embed[F[_]: FlatMap](ft: F[Stream[F, A]]): Stream[F, A]      = fs2.Stream.force(ft)
  def mapK[F[_], G[_]](af: Stream[F, A])(fk: F ~> G): Stream[G, A] = af.translate(fk)
}

class FS2Context[F[_], R](implicit ctx: F HasContext R) extends Context[Stream[F, *]] {
  val functor: Functor[Stream[F, *]] = implicitly
  type Ctx = R
  def context: Stream[F, R] = Stream.eval(ctx.context)
}
