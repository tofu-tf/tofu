package tofu.syntax

import cats.{ApplicativeError, MonadError}
import cats.data.EitherT
import cats.syntax.applicativeError._
import cats.syntax.either._
import cats.syntax.functor._
import either._

object monadError {
  implicit final class MonadErrorFOps[F[_], A](private val fa: F[A]) extends AnyVal {
    def retryAttempt[E](count: Int)(implicit F: MonadError[F, E]): F[Either[List[E], A]] =
      F.tailRecM((count, List.empty[E])) { case (cnt, acc) =>
        if (cnt <= 0) F.pure(acc.reverse.asLeft.asRight)
        else
          fa.attempt.map {
            case Left(err) => (cnt - 1, err :: acc).asLeft
            case Right(v)  => v.asRight.asRight
          }
      }

    def retry[E](count: Int)(implicit F: MonadError[F, E]): F[A] =
      F.tailRecM(count) {
        case cnt if cnt <= 1 => fa.map(_.asRight)
        case cnt             => fa.attempt.map(_.leftMap(_ => cnt - 1))
      }
  }

  implicit final class MonadErrorEitherTOps[F[_], E, A](private val fea: EitherT[F, E, A]) extends AnyVal {

    /** sums logic error with underlying error */
    def attemptXor[U](implicit F: ApplicativeError[F, U]): EitherT[F, Either[U, E], A] =
      EitherT(fea.value.attempt.map(_.assocR))
  }
}
