package tofu.optics
package indexed
import cats.Applicative
import tofu.syntax.monadic._

trait IPProperty[+I, -S, +T, +A, -B]
    extends PProperty[S, T, A, B] with IPItems[I, S, T, A, B] with IPDowncast[I, S, T, A, B]
    with IPUpdate[I, S, T, A, B] {


  def inarrow(s: S): Either[T, (I, A)]

  override def narrow(s: S): Either[T, A] = inarrow(s).map(_._2)

  override def idowncast(s: S): Option[(I, A)] = inarrow(s).toOption

  override def itraverse[F[+_]](s: S)(f: (I, A) => F[B])(implicit F: Applicative[F]): F[T] =
    inarrow(s) match {
      case Left(t)       => F.pure(t)
      case Right((i, a)) => f(i, a).map(b => set(s, b))
    }
}
