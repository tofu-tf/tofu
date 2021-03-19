package tofu
package memo

import cats.effect.concurrent.{MVar, Ref}
import cats.tagless.InvariantK
import cats.{Functor, Monad, ~>}
import tofu.concurrent.{MVars, MakeMVar, MakeRef, Refs}
import tofu.memo.CacheOperation.{CleanUp, GetOrElse}
import tofu.syntax.bracket._
import tofu.syntax.monadic._

import scala.annotation.nowarn

abstract class CacheState[F[_], A] {
  def runOperation[B](op: CacheOperation[F, A, B]): F[B]
  def value: F[CacheVal[A]]

  def getOrElse(process: F[A], now: Long, after: Long): F[A] = runOperation(GetOrElse(process, now, after))
  def cleanUp(after: Long): F[Boolean]                       = runOperation(CleanUp(after))
}

object CacheState {
  def apply[F[_]: Monad: Guarantee: Refs: MVars, A](
      method: CacheMethod,
      initial: CacheVal[A] = CacheVal.None
  ): F[CacheState[F, A]] =
    in[F, F, A](method, initial)

  def in[I[_]: Functor, F[_]: Monad: Guarantee, A](method: CacheMethod, initial: CacheVal[A] = CacheVal.None)(implicit
      mvar: MakeMVar[I, F],
      refs: MakeRef[I, F]
  ): I[CacheState[F, A]] =
    method match {
      case CacheMethod.MVar => mvarIn[I, F, A](initial)
      case CacheMethod.Ref  => refIn[I, F, A](initial)
    }

  def mvarIn[I[_]: Functor, F[_]: Monad: Guarantee, A](initial: CacheVal[A] = CacheVal.none)(implicit
      mvars: MakeMVar[I, F]
  ): I[CacheState[F, A]] = mvars.mvarOf(initial).map(CacheStateMVar(_))

  def refIn[I[_]: Functor, F[_]: Monad, A](
      initial: CacheVal[A] = CacheVal.none
  )(implicit refs: MakeRef[I, F]): I[CacheState[F, A]] =
    refs.refOf(initial).map(CacheStateRef(_))

  def mvar[F[_]: Monad: Guarantee: MVars, A](initial: CacheVal[A] = CacheVal.none): F[CacheState[F, A]] =
    mvarIn[F, F, A](initial)

  def ref[F[_]: Monad: Refs, A](initial: CacheVal[A] = CacheVal.none): F[CacheState[F, A]] = refIn[F, F, A](initial)

  implicit def invariantK[A]: InvariantK[CacheState[*[_], A]] = new InvariantK[CacheState[*[_], A]] {
    def imapK[F[_], G[_]](af: CacheState[F, A])(fk: F ~> G)(gk: G ~> F): CacheState[G, A] =
      new CacheState[G, A] {
        def runOperation[B](op: CacheOperation[G, A, B]): G[B] = fk(af.runOperation(op.mapK(gk)))
        def value: G[CacheVal[A]]                              = fk(af.value)
      }
  }
}

@nowarn("cat=deprecation")
final case class CacheStateMVar[F[_]: Monad: Guarantee, A](state: MVar[F, CacheVal[A]]) extends CacheState[F, A] {
  override def value: F[CacheVal[A]]                              = state.read
  override def runOperation[B](op: CacheOperation[F, A, B]): F[B] =
    for {
      cur <- state.read
      res <- op.getPureOrElse(cur)(
               state.bracketModify(fresh => op.update(fresh))
             )
    } yield res
}

final case class CacheStateRef[F[_]: Monad, A](state: Ref[F, CacheVal[A]]) extends CacheState[F, A] {
  override def value: F[CacheVal[A]]                              = state.get
  override def runOperation[B](op: CacheOperation[F, A, B]): F[B] =
    for {
      (cur, update) <- state.access
      res           <- op.getPureOrElse(cur)(
                         for {
                           (newVal, res) <- op.update(cur)
                           _             <- update(newVal)
                         } yield res
                       )
    } yield res
}
