package tofu.syntax

import cats.Applicative
import cats.data.EitherT
import cats.effect.concurrent.MVar
import cats.effect.{Bracket, ExitCase}
import cats.effect.syntax.bracket._
import cats.syntax.either._
import tofu.Guarantee
import tofu.syntax.monadic._

import scala.annotation.nowarn

object bracket extends GuaranteeSyntax {
  @nowarn("cat=deprecation")
  implicit final class TofuBracketMVarOps[F[_], A](private val mvar: MVar[F, A]) extends AnyVal {

    /** Update value with effectful transformation. In case of error or cancellation value remains unchanged
      * @param use
      *   function to atomically modify value contained in `MVar`
      * @return
      *   `F[A]` modified value contained in `MVar`
      */
    def bracketUpdate(use: A => F[A])(implicit FG: Guarantee[F], A: Applicative[F]): F[A] =
      mvar.take.bracketReplace(use)(mvar.put)

    /** Modify value with effectful transformation, calculating result. In case of error or cancellation value remains
      * unchanged
      * @param use
      *   function to atomically modify value contained in `MVar` and produce result
      * @return
      *   `F[B]`
      */
    def bracketModify[B](use: A => F[(A, B)])(implicit FG: Guarantee[F], A: Applicative[F]): F[B] =
      mvar.take.bracketState(use)(mvar.put)
  }

  implicit final class TofuBracketEitherTOps[F[_], E, A](private val e: EitherT[F, E, A]) extends AnyVal {

    /** special bracket form that could handle both Either logic error and F underlying error */
    def bracketCaseErr[U, B](
        use: A => EitherT[F, E, B]
    )(release: (A, ExitCase[Either[E, U]]) => F[Unit])(implicit bracket: Bracket[F, U]): EitherT[F, E, B] =
      EitherT(
        e.value.bracketCase[Either[E, B]] {
          // could not acquire resource
          case Left(err)  => bracket.pure(err.asLeft[B])
          // case logic error
          case Right(res) => use(res).leftSemiflatMap(e => release(res, ExitCase.error(e.asLeft[U])) as e).value
        }((res, cas) =>
          res match {
            case Left(_)  => bracket.unit
            case Right(v) =>
              cas match {
                // case F underlying error
                case ExitCase.Error(e)  => release(v, ExitCase.error(e.asRight[E]))
                case ExitCase.Canceled  => release(v, ExitCase.Canceled)
                case ExitCase.Completed => release(v, ExitCase.Completed)
              }
          }
        )
      )

    /** special bracket form that could handle both Either logic error and F underlying error */
    def bracketIncompleteErr[U, B](
        use: A => EitherT[F, E, B]
    )(release: A => F[Unit])(implicit bracket: Bracket[F, U]): EitherT[F, E, B] =
      bracketCaseErr[U, B](use) { (a, cas) =>
        cas match {
          case ExitCase.Completed => bracket.unit
          case _                  => release(a)
        }
      }
  }
}
