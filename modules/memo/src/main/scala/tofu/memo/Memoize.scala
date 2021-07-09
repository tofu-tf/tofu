package tofu.memo

import cats.effect.{Concurrent, ExitCase}
import cats.effect.concurrent.{Deferred, Ref}
import simulacrum.typeclass
import tofu.syntax.monadic._
import cats.syntax.option._
import cats.effect.syntax.concurrent._
import cats.effect.syntax.bracket._

import scala.annotation.nowarn

/** WARNING breaks referential transparency, use with great care */
@typeclass @nowarn("cat=unused-imports")
trait Memoize[F[_]] {
  def memoize[A](fa: F[A]): F[F[A]]

  /** should be redefined if F is at least ApplicativeError */
  def memoizeOnSuccess[A](fa: F[A]): F[F[A]]
}

object Memoize {
  def concurrentMemoize[F[_]](implicit F: Concurrent[F]): Memoize[F] =
    new Memoize[F] {
      def memoize[A](fa: F[A]): F[F[A]] = Concurrent.memoize(fa)

      //copy of Concurrent.memoize accepting success only
      def memoizeOnSuccess[A](f: F[A]): F[F[A]] = {
        {
          sealed trait State
          case class Subs(n: Int) extends State
          case object Done        extends State

          case class Fetch(state: State, v: Deferred[F, A], stop: Deferred[F, F[Unit]])

          Ref[F].of(Option.empty[Fetch]).map { state =>
            (Deferred[F, A] product Deferred[F, F[Unit]]).flatMap { case (v, stop) =>
              def endState(ec: ExitCase[Throwable]) =
                state.modify {
                  case None                          => throw new AssertionError("unreachable")
                  case s @ Some(Fetch(Done, _, _))   => s -> F.unit
                  case Some(Fetch(Subs(n), v, stop)) =>
                    if (ec == ExitCase.Canceled && n == 1) None -> stop.get.flatten
                    else if (ec == ExitCase.Canceled) Fetch(Subs(n - 1), v, stop).some -> F.unit
                    else Fetch(Done, v, stop).some                                     -> F.unit
                }.flatten

              def fetch =
                f.flatMap(v.complete)
                  .start
                  .flatMap(fiber => stop.complete(fiber.cancel))

              state.modify {
                case s @ Some(Fetch(Done, v, _))   =>
                  s -> v.get
                case Some(Fetch(Subs(n), v, stop)) =>
                  Fetch(Subs(n + 1), v, stop).some -> v.get.guaranteeCase(endState)
                case None                          =>
                  Fetch(Subs(1), v, stop).some -> fetch.bracketCase(_ => v.get) { case (_, ec) => endState(ec) }
              }.flatten
            }
          }
        }
      }
    }
}
