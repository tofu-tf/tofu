package tofu.concurrent

import cats.{Applicative, Eval, Traverse}
import tofu.control.ApplicativeZip
import tofu.syntax.monadic._
import scala.util.Try
import tofu.concurrent.Exit.Canceled
import tofu.concurrent.Exit.Completed
import tofu.concurrent.Exit.Error

sealed trait Exit[+E, +A] {
  def toTry(implicit ev: E <:< Throwable): Try[A] = this match {
    case Canceled     => util.Failure(new InterruptedException)
    case Error(e)     => util.Failure(e)
    case Completed(a) => util.Success(a)
  }

  def toEither: Either[Option[E], A] = this match {
    case Canceled     => Left(None)
    case Error(e)     => Left(Some(e))
    case Completed(a) => Right(a)
  }
}

object Exit {

  sealed trait Incomplete[+E] extends Exit[E, Nothing]

  case object Canceled                 extends Incomplete[Nothing]
  final case class Error[+E](e: E)     extends Incomplete[E]
  final case class Completed[+A](a: A) extends Exit[Nothing, A]

  def fromEither[E, A](e: Either[E, A]): Exit[E, A] = e match {
    case Left(err)  => Error(err)
    case Right(res) => Completed(res)
  }

  def fromTry[A](t: util.Try[A]): Exit[Throwable, A] = t match {
    case util.Failure(ex)  => Error(ex)
    case util.Success(res) => Completed(res)
  }

  private[this] object exitInstanceAny extends Traverse[Exit[Any, *]] with ApplicativeZip[Exit[Any, *]] {
    def traverse[G[_], A, B](fa: Exit[Any, A])(f: A => G[B])(implicit G: Applicative[G]): G[Exit[Any, B]] =
      fa match {
        case Canceled     => G.pure(Canceled)
        case Error(e)     => G.pure(Error(e))
        case Completed(a) => f(a).map(Completed(_))
      }
    def foldLeft[A, B](fa: Exit[Any, A], b: B)(f: (B, A) => B): B                                         =
      fa match {
        case Canceled | Error(_) => b
        case Completed(a)        => f(b, a)
      }
    def foldRight[A, B](fa: Exit[Any, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B]               =
      fa match {
        case Canceled | Error(_) => lb
        case Completed(a)        => f(a, lb)
      }
    override def map[A, B](fa: Exit[Any, A])(f: A => B): Exit[Any, B]                                     =
      fa match {
        case Canceled      => Canceled
        case e: Error[Any] => e
        case Completed(a)  => Completed(f(a))
      }

    def zipWith[A, B, C](fa: Exit[Any, A], fb: Exit[Any, B])(f: (A, B) => C): Exit[Any, C] =
      fa match {
        case Canceled        => Canceled
        case err: Error[Any] => err
        case Completed(a)    =>
          fb match {
            case Canceled        => Canceled
            case err: Error[Any] => err
            case Completed(b)    => Completed(f(a, b))
          }
      }
    def pure[A](x: A): Exit[Any, A]                                                        = Completed(x)
  }

  implicit def exitInstance[E]: Traverse[Exit[E, *]] with Applicative[Exit[E, *]] =
    exitInstanceAny.asInstanceOf[Traverse[Exit[E, *]] with Applicative[Exit[E, *]]]

  object CanceledException extends InterruptedException
}
