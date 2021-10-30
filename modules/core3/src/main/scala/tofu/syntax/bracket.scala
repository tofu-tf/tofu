package tofu.syntax

import cats.data.EitherT
import cats.effect.MonadCancel
import cats.effect.kernel.Outcome
import cats.effect.syntax.monadCancel._
import cats.syntax.either._
import tofu.syntax.monadic._

object bracket extends GuaranteeSyntax {

  implicit final class TofuBracketEitherTOps[F[_], E, A](private val e: EitherT[F, E, A]) extends AnyVal {

    /** special bracket form that could handle both Either logic error and F underlying error */
    def bracketCaseErr[U, B](
        use: A => EitherT[F, E, B]
    )(release: (A, Outcome[F, Either[E, U], B]) => F[Unit])(implicit bracket: MonadCancel[F, U]): EitherT[F, E, B] =
      EitherT(
        e.value.bracketCase[Either[E, B]] {
          // could not acquire resource
          case Left(err)  => bracket.pure(err.asLeft[B])
          // case logic error
          case Right(res) => use(res).leftSemiflatMap(e => release(res, Outcome.errored(e.asLeft[U])) as e).value
        }((res, cas) =>
          res match {
            case Left(_)  => bracket.unit
            case Right(v) =>
              cas match {
                // case F underlying error
                case Outcome.Errored(e)    => release(v, Outcome.errored(e.asRight[E]))
                case Outcome.Canceled()    => release(v, Outcome.Canceled())
                case Outcome.Succeeded(fa) =>
                  fa.flatMap {
                    case Left(e)  => release(v, Outcome.errored(e.asLeft[U]))
                    case Right(b) => release(v, Outcome.succeeded(bracket.pure(b)))
                  }

              }
          }
        )
      )

    /** special bracket form that could handle both Either logic error and F underlying error */
    def bracketIncompleteErr[U, B](
        use: A => EitherT[F, E, B]
    )(release: A => F[Unit])(implicit bracket: MonadCancel[F, U]): EitherT[F, E, B] =
      bracketCaseErr[U, B](use) { (a, cas) =>
        cas match {
          case Outcome.Succeeded(_) => bracket.unit
          case _                    => release(a)
        }
      }
  }
}
