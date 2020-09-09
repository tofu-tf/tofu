package tofu.memo

import cats.data.OptionT
import cats.effect.concurrent.{MVar, Ref}
import cats.instances.list._
import cats.instances.option._
import cats.syntax.foldable._
import cats.syntax.traverse._
import cats.{Functor, Monad}
import tofu.Guarantee
import tofu.concurrent.{MVars, MakeMVar, MakeRef, Refs}
import tofu.memo.CacheKeyState.valueByMap
import tofu.memo.CacheOperation.{CleanUp, GetOrElse}
import tofu.syntax.bracket._
import tofu.syntax.monadic._

import scala.annotation.nowarn

trait CacheKeyState[F[_], -K, A] {
  def runOperation[B](key: K, op: CacheOperation[F, A, B]): F[B]
  def getOrElse(process: F[A], key: K, now: Long, after: Long): F[A] =
    runOperation(key, GetOrElse(process, now, after))
  def cleanUpSingle(key: K, after: Long): F[Boolean]                 =
    runOperation(key, CleanUp(after))
  def cleanUp(after: Long): F[Long]
  def value(key: K): F[CacheVal[A]]
}

object CacheKeyState {
  private[memo] def valueByMap[F[_]: Monad, K, A](mapf: F[Map[K, CacheState[F, A]]])(key: K): F[CacheVal[A]] =
    OptionT.liftF(mapf).subflatMap(_.get(key)).semiflatMap(_.value).getOrElse(CacheVal.none)

  type CacheMap[F[_], K, A] = Map[K, CacheState[F, A]]

  def apply[I[_]: Functor, F[_]: Monad: Guarantee: Refs: MVars, K, A](
      keyMethod: CacheMethod,
      valueMethod: CacheMethod
  )(implicit refs: MakeRef[I, F], mvars: MakeMVar[I, F]): I[CacheKeyState[F, K, A]] =
    apply[I, F, K, A](keyMethod)(CacheState(valueMethod, _))

  def apply[I[_]: Functor, F[_]: Monad: Guarantee, K, A](keyMethod: CacheMethod)(
      factory: CacheVal[A] => F[CacheState[F, A]]
  )(implicit refs: MakeRef[I, F], mvars: MakeMVar[I, F]): I[CacheKeyState[F, K, A]] =
    keyMethod match {
      case CacheMethod.MVar => mvar[I, F, K, A](factory)
      case CacheMethod.Ref  => ref[I, F, K, A](factory)
    }

  def mvar[I[_]: Functor, F[_]: Monad: Guarantee, K, A](
      factory: CacheVal[A] => F[CacheState[F, A]]
  )(implicit mvars: MakeMVar[I, F]): I[CacheKeyState[F, K, A]] =
    mvars.mvarOf[CacheMap[F, K, A]](Map.empty).map(CacheKeyStateMVar(_, factory))

  def ref[I[_]: Functor, F[_]: Monad, K, A](
      factory: CacheVal[A] => F[CacheState[F, A]]
  )(implicit refs: MakeRef[I, F]): I[CacheKeyState[F, K, A]] =
    refs.refOf[CacheMap[F, K, A]](Map.empty).map(CacheKeyStateRef(_, factory))
}

@nowarn("cat=deprecation")
final case class CacheKeyStateMVar[F[_]: Monad: Guarantee, K, A](
    state: MVar[F, Map[K, CacheState[F, A]]],
    factory: CacheVal[A] => F[CacheState[F, A]]
) extends CacheKeyState[F, K, A] {
  override def value(key: K): F[CacheVal[A]] = valueByMap(state.read)(key)
  override def runOperation[B](key: K, op: CacheOperation[F, A, B]): F[B] = {
    def miss: F[B] =
      op.getPureOrElse(CacheVal.none)(state.bracketModify(freshMap => {
        freshMap.get(key).map(_.runOperation(op).map((freshMap, _))).getOrElse {
          for {
            (newVal, res) <- op.update(CacheVal.none)
            newMap        <- newVal.fold(Monad[F].pure(freshMap))((_, _) =>
                               factory(newVal) >>= (cell => Monad[F].pure(freshMap + (key -> cell)))
                             )
          } yield (newMap, res)
        }
      }))

    for {
      map <- state.read
      res <- map.get(key).fold(miss)(_.runOperation(op))
    } yield res
  }

  override def cleanUp(after: Long): F[Long] =
    state.bracketModify { map =>
      for {
        results  <- map.toList.traverse { case (k, v) => v.cleanUp(after).tupleLeft(k) }
        cleanKeys = results.collect { case (k, true) => k }
      } yield (map -- cleanKeys, cleanKeys.size.toLong)
    }
}

final case class CacheKeyStateRef[F[_]: Monad, K, A](
    state: Ref[F, Map[K, CacheState[F, A]]],
    factory: CacheVal[A] => F[CacheState[F, A]]
) extends CacheKeyState[F, K, A] {

  override def value(key: K): F[CacheVal[A]]                              = valueByMap(state.get)(key)
  override def runOperation[B](key: K, op: CacheOperation[F, A, B]): F[B] =
    for {
      (map, update) <- state.access
      res           <- map.get(key).map(_.runOperation(op)).getOrElse {
                         op.getPureOrElse(CacheVal.none)(
                           for {
                             (newVal, res) <- op.update(CacheVal.none)
                             _             <- newVal.getOption.traverse_(_ => factory(newVal) >>= (cell => update(map + (key -> cell))))
                           } yield res
                         )
                       }
    } yield res

  override def cleanUp(after: Long): F[Long] =
    for {
      (map, update) <- state.access
      results       <- map.toList.traverse { case (k, v) => v.cleanUp(after).tupleLeft(k) }
      cleanKeys      = results.collect { case (k, true) => k }
      _             <- update(map -- cleanKeys)
    } yield cleanKeys.size.toLong
}
