package tofu.syntax

import cats.{Applicative, Monad}
import cats.data.EitherT
import cats.effect.concurrent.MVar
import cats.effect.{Bracket, ExitCase}
import cats.effect.syntax.bracket._
import cats.syntax.functor._
import cats.syntax.applicative._
import cats.syntax.either._
import tofu.{Finally, Guarantee}

object bracket {
  implicit final class TofuBracketOps[F[_], A](private val fa: F[A]) extends AnyVal {
    def bracketIncomplete[B, C](
        use: A => F[B]
    )(release: A => F[C])(implicit F: Applicative[F], FG: Guarantee[F]): F[B]                     =
      FG.bracket(fa)(use) { case (a, success) => release(a).whenA(!success) }

    def bracketAlways[B, C](
        use: A => F[B]
    )(release: A => F[C])(implicit FG: Guarantee[F]): F[B]                                        =
      FG.bracket(fa)(use) { case (a, _) => release(a) }

    def guaranteeIncomplete[B](release: F[B])(implicit F: Applicative[F], FG: Guarantee[F]): F[A] =
      FG.bracket(F.unit)(_ => fa)((_, success) => release.whenA(!success))

    def guaranteeAlways[B](release: F[B])(implicit F: Applicative[F], FG: Guarantee[F]): F[A] =
      FG.bracket(F.unit)(_ => fa)((_, _) => release)

    def guaranteeOpt[B](use: A => F[A])(release: A => F[B])(implicit FG: Guarantee[F], A: Applicative[F]): F[A] =
      FG.bracket(FG.bracket(fa)(use) {
        case (oldA, success) => release(oldA).whenA(!success)
      })(newA => release(newA).map(_ => newA)) { (_, _) => A.unit }

    def guaranteeModifyOpt[B, C](
        use: A => F[(A, B)]
    )(release: A => F[C])(implicit FG: Guarantee[F], A: Applicative[F]): F[B]                            =
      FG.bracket(FG.bracket(fa)(use) {
        case (oldA, success) => release(oldA).whenA(!success)
      }) { case (newA, b) => release(newA).map(_ => b) } { (_, _) => A.unit }

    def bracketOpt[B, C](use: A => F[B])(release: (A, Boolean) => F[C])(implicit FG: Guarantee[F]): F[B] =
      FG.bracket(fa)(use)(release)

    def finallyCase[Ex[_], B, C](use: A => F[B])(release: (A, Ex[B]) => F[C])(implicit FG: Finally[F, Ex]): F[B] =
      FG.finallyCase(fa)(use)(release)
  }

  implicit final class TofuBracketMVarOps[F[_], A](private val mvar: MVar[F, A]) extends AnyVal {

    /**
      * Update value with effectful transformation. In case of error value remains unchanged
      * @param use function to atomically modify value contained in `MVar`
      * @return `F[A]` modified value contained in `MVar`
      */
    def bracketUpdate(use: A => F[A])(implicit FG: Guarantee[F], A: Applicative[F]): F[A] =
      mvar.take.guaranteeOpt(use)(mvar.put)

    /**
      * Modify value with effectful transformation, calculating result. In case of error value remains unchanged
      * @param use function to atomically modify value contained in `MVar` and produce result
      * @return `F[B]`
      */
    def bracketModify[B](use: A => F[(A, B)])(implicit FG: Guarantee[F], A: Applicative[F]): F[B] =
      mvar.take.guaranteeModifyOpt(use)(mvar.put)
  }

  implicit final class TofuBracketEitherTOps[F[_], E, A](private val e: EitherT[F, E, A]) extends AnyVal {

    /** special bracket form that could handle both Either logic error and F underlying error */
    def bracketCaseErr[U, B](
        use: A => EitherT[F, E, B]
    )(release: (A, ExitCase[Either[E, U]]) => F[Unit])(implicit bracket: Bracket[F, U]): EitherT[F, E, B] =
      EitherT(
        e.value.bracketCase[Either[E, B]] {
          //could not acquire resource
          case Left(err)  => bracket.pure(err.asLeft[B])
          //case logic error
          case Right(res) => use(res).leftSemiflatMap(e => release(res, ExitCase.error(e.asLeft[U])) as e).value
        }((res, cas) =>
          res match {
            case Left(_)  => bracket.unit
            case Right(v) =>
              cas match {
                //case F underlying error
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
