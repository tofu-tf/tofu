package tofu
package config

import cats.effect.Sync
import cats.instances.list._
import cats.instances.option._
import cats.syntax.option._
import cats.{Applicative, Functor, Id, Monad, ~>}
import tofu.concurrent.Refs
import tofu.config.ConfigItem.ValueType
import tofu.data.{Flux, Identity}
import tofu.syntax.monadic._

import collection.immutable.IndexedSeq
import scala.annotation.unchecked.{uncheckedVariance => uv}

sealed trait ConfigItem[+F[_]] {
  self =>
  type T[+_[_]]
  def valueType: ValueType[T]
  def value: T[F]
  def mapK[G[_]: Functor](fk: (F @uv) ~> G): ConfigItem[G]
}

object ConfigItem {
  sealed trait Value[+F[_], U[_[_]]] extends ConfigItem[F]                    {
    type T[f[_]] = U[f]
  }
  sealed trait SimpleValue[A]        extends Value[Nothing, SimpleC[*[_], A]] {
    def valueType: ValueTypeSimple[A]
    def mapK[G[_]: Functor](fk: Nothing ~> G): ConfigItem[G] = this
  }
  sealed trait IndexedValue[F[_], I] extends Value[F, IndexedC[*[_], I]]      {
    self =>
    def valueType: ValueTypeIndexed[I]
    def keys: Flux.Stream[F, I]
    def get: I => F[ConfigItem[F]]
    def value                                          = (get, keys)
    def mapK[G[_]: Functor](fk: F ~> G): ConfigItem[G] = new IndexedValue[G, I] {
      def valueType = self.valueType
      def keys      = self.keys.mapK(fk)
      def get       = i => fk(self.get(i)).map(_.mapK(fk))
    }
  }

  sealed trait ValueTag
  sealed trait ValueType[T[_[_]]] extends ValueTag {
    def unapply[F[_]](item: ConfigItem[F]): Option[T[F]] =
      if (item.valueType == this) Some(item.value.asInstanceOf[T[F]])
      else None
  }

  type IndexedC[f[_], I] = (I => f[ConfigItem[f]], Flux.Stream[f, I])
  type StreamC[f[_]]     = Flux.Stream[f, ConfigItem[f]]
  type SimpleC[f[_], A]  = A

  sealed trait ValueTypeSimple[A]  extends ValueType[SimpleC[*[_], A]]
  sealed trait ValueTypeIndexed[I] extends ValueType[IndexedC[*[_], I]]
  sealed trait ValueTypeStream     extends ValueType[StreamC]

  object ValueType {
    case object Null extends ValueTypeSimple[Unit]
    case object Bool extends ValueTypeSimple[Boolean]
    case object Num  extends ValueTypeSimple[BigDecimal]
    case object Str  extends ValueTypeSimple[String]

    case object Stream extends ValueTypeStream
    case object Array  extends ValueTypeIndexed[Int]
    case object Dict   extends ValueTypeIndexed[String]
  }

  case object Null                        extends SimpleValue[Unit]       {
    def value     = ()
    def valueType = ValueType.Null
  }
  final case class Bool(value: Boolean)   extends SimpleValue[Boolean]    {
    def valueType = ValueType.Bool
  }
  final case class Num(value: BigDecimal) extends SimpleValue[BigDecimal] {
    def valueType = ValueType.Num
  }
  final case class Str(value: String)     extends SimpleValue[String]     {
    def valueType = ValueType.Str
  }

  final case class Stream[F[_]](value: Flux.Stream[F, ConfigItem[F]]) extends Value[F, StreamC] {
    def valueType                                      = ValueType.Stream
    def mapK[G[_]: Functor](fk: F ~> G): ConfigItem[G] = Stream(value.mapK(fk).map(_.mapK(fk)))
  }

  final case class Array[F[_]](get: Int => F[ConfigItem[F]], keys: Flux.Stream[F, Int]) extends IndexedValue[F, Int] {
    def valueType = ValueType.Array
  }

  final case class Dict[F[_]](get: String => F[ConfigItem[F]], keys: Flux.Stream[F, String])
      extends IndexedValue[F, String] {
    def valueType = ValueType.Dict
  }

  private def indexed[F[_]: Applicative, I](get: I => Option[ConfigItem[F]])(i: I): F[ConfigItem[F]] =
    get(i).fold((Null: ConfigItem[F]).pure[F])(_.pure[F])

  def seq[F[_]: Applicative](seq: IndexedSeq[ConfigItem[F]]): Array[F] =
    Array[F](indexed(seq.lift), Flux.Stream.range[F, Int](0, seq.size))

  def dict[F[_]: Applicative](dict: Map[String, ConfigItem[F]]): Dict[F] =
    Dict[F](indexed(dict.lift), Flux.Stream[F](dict.keys.toList))

  implicit final class SyncConfigItemOps[F[_]](private val item: ConfigItem[F]) extends AnyVal {
    def tryParseSync[A: Configurable](implicit
        FR: Refs[F],
        FM: Monad[F],
        FE: ErrorsFail[F],
        PR: ParallelReader[F]
    ): F[(MessageList, Option[A])] =
      ConfigMonad.tryParse[F, A](item)

    def parseSync[A: Configurable](implicit FR: Refs[F], F: MonadThrow[F], PR: ParallelReader[F]): F[A] =
      for {
        (mess, optA) <- tryParseSync[A]
        res          <- optA.liftTo[F](ConfigParseErrors(mess))
      } yield res
  }

  implicit final class IdConfigItemOps(private val item: ConfigItem[Identity]) extends AnyVal {
    def liftF[F[_]](implicit F: Applicative[F]): ConfigItem[F]              =
      item.mapK(new (Id ~> F) { def apply[A](x: A) = F.pure(x) })
    def tryParseF[F[_]: Sync, A: Configurable]: F[(MessageList, Option[A])] = liftF[F].tryParseSync[A]
    def parseF[F[_]: Sync, A: Configurable]: F[A]                           = liftF[F].parseSync[A]
  }
}
