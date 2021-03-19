package tofu.syntax

import cats.Applicative
import cats.data.EitherT
import cats.effect.concurrent.MVar
import cats.effect.{Bracket, ExitCase}
import cats.effect.syntax.bracket._
import cats.syntax.either._
import tofu.{Finally, Guarantee}
import tofu.syntax.monadic._

import scala.annotation.nowarn

object bracket {
  implicit class TofuBracketOps[F[_], A](val fa: F[A]) extends AnyVal {
    def guaranteeIf[B](fb: Boolean => F[B])(implicit FG: Guarantee[F], F: Applicative[F]) =
      FG.bracket(F.unit)(_ => fa)((_, b) => fb(b))

    /** Apply function to [[fa]] with effectful transformation. In case of error or cancellation [[fa]] is released
      * @param use function to modify value contained in [[fa]]
      * @param release function to release value contained in [[fa]]
      * @return `F[B]` updated value
      */
    def bracketIncomplete[B, C](
        use: A => F[B]
    )(release: A => F[C])(implicit F: Applicative[F], FG: Guarantee[F]): F[B]                     =
      FG.bracket(fa)(use) { case (a, success) => success unless_ release(a) }

    /** Apply function to [[fa]] with effectful transformation. [[fa]] is always released
      * @param use function to modify value contained in [[fa]]
      * @param release function to release value contained in [[fa]]
      * @return `F[B]` updated value
      */
    def bracketAlways[B, C](
        use: A => F[B]
    )(release: A => F[C])(implicit FG: Guarantee[F]): F[B]                                        =
      FG.bracket(fa)(use) { case (a, _) => release(a) }

    /** Guarantee finalization of [[fa]]. `release` is called in case of error or cancellation
      * @param release function to release value contained in [[fa]]
      * @return `F[B]` updated value
      */
    def guaranteeIncomplete[B](release: F[B])(implicit F: Applicative[F], FG: Guarantee[F]): F[A] =
      FG.bracket(F.unit)(_ => fa)((_, success) => success unless_ release)

    /** Guarantee finalization of [[fa]]. `release` is alwyas called
      * @param release function to release value contained in [[fa]]
      * @return `F[A]` updated value
      */
    def guaranteeAlways[B](release: F[B])(implicit F: Applicative[F], FG: Guarantee[F]): F[A] =
      FG.bracket(F.unit)(_ => fa)((_, _) => release)

    /** Guarantee finalization of [[fa]].
      * `release` is always called and boolean indicating success of usage is passed to it
      * @param release function to release value contained in [[fa]] depending on success of usage
      * @return `F[A]` updated value
      */
    def guaranteeOpt[B](release: Boolean => F[B])(implicit F: Applicative[F], FG: Guarantee[F]): F[A] =
      FG.bracket(F.unit)(_ => fa)((_, success) => release(success))

    /** Replace value in [[fa]]. In case of error or cancellation old value of [[fa]] is passed to `commit`, otherwise
      * result of `use` is passed.
      * @param use function to modify value contained in [[fa]]
      * @param commit function to commit passed value
      * @return `F[A]` updated value
      */
    def bracketReplace[B](use: A => F[A])(commit: A => F[B])(implicit FG: Guarantee[F], A: Applicative[F]): F[A] =
      FG.bracket(FG.bracket(fa)(use) { case (oldA, success) =>
        success unless_ commit(oldA)
      })(newA => commit(newA) as newA) { (_, _) => A.unit }

    /** Use value contained in [[fa]]. `release` is guaranteed to be called
      * @param use function to modify value contained in [[fa]]
      * @param release function to release value contained in [[fa]]
      * @return `F[B]` updated value
      */
    def bracketOpt[B, C](use: A => F[B])(release: (A, Boolean) => F[C])(implicit FG: Guarantee[F]): F[B] =
      FG.bracket(fa)(use)(release)

    /** Update value in [[fa]] while producing additional one.
      * In case of error or cancellation old value of [[fa]] is passed to `commit`, otherwise
      * result of `use` is passed.
      * @param use function to modify value contained in [[fa]] and produce additional one
      * @param commit function to commit passed value
      * @return `F[B]` result of `use`
      */
    def bracketState[B, C](
        use: A => F[(A, B)]
    )(commit: A => F[C])(implicit FG: Guarantee[F], A: Applicative[F]): F[B]                                     =
      FG.bracket(FG.bracket(fa)(use) { case (oldA, success) =>
        success unless_ commit(oldA)
      }) { case (newA, b) => commit(newA) as b } { (_, _) => A.unit }

    /** Update value in [[fa]] and then release with respect to exit cause.
      * @param use function to modify value contained in [[fa]]
      * @param release function to release value depending on exit cause
      * @return `F[B]` modified value
      */
    def finallyCase[Ex[_], B, C](use: A => F[B])(release: (A, Ex[B]) => F[C])(implicit FG: Finally[F, Ex]): F[B] =
      FG.finallyCase(fa)(use)(release)
  }

  @nowarn("cat=deprecation")
  implicit final class TofuBracketMVarOps[F[_], A](private val mvar: MVar[F, A]) extends AnyVal {

    /** Update value with effectful transformation. In case of error or cancellation value remains unchanged
      * @param use function to atomically modify value contained in `MVar`
      * @return `F[A]` modified value contained in `MVar`
      */
    def bracketUpdate(use: A => F[A])(implicit FG: Guarantee[F], A: Applicative[F]): F[A] =
      mvar.take.bracketReplace(use)(mvar.put)

    /** Modify value with effectful transformation, calculating result. In case of error or cancellation value remains unchanged
      * @param use function to atomically modify value contained in `MVar` and produce result
      * @return `F[B]`
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
