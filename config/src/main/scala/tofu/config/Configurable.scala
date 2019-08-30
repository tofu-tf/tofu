package tofu
package config

import java.net.{URI, URL}

import ConfigMonad.promote._
import cats.data.{Chain, NonEmptyList}
import cats.instances.list._
import cats.instances.map._
import cats.instances.option._
import cats.instances.vector._
import cats.kernel.Monoid
import cats.syntax.alternative._
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.option._
import cats.syntax.parallel._
import magnolia.{CaseClass, Magnolia, SealedTrait}
import syntax.handle._
import tofu.config.ConfigItem._
import syntax.context._
import tofu.config.Key.{Index, Prop, Variant}

import scala.collection.immutable.IndexedSeq
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.util.control.NonFatal

trait Configurable[A] extends ConfigArr[ConfigItem, A] { self =>
  import ConfigMonad.promote._
  def apply[F[_]: ConfigMonad](cfg: ConfigItem[F]): F[A]

  def map[B](f: A => B): Configurable[B] = new Configurable[B] {
    def apply[F[_]: ConfigMonad](cfg: ConfigItem[F]): F[B] = self(cfg).map(f)
  }
  def flatMap[B](f: ConfigFunc[A, B]): Configurable[B] = new Configurable[B] {
    def apply[F[_]: ConfigMonad](cfg: ConfigItem[F]): F[B] =
      self(cfg).flatMap(f[F](_))
  }

  def flatMake[B] =
    ConfigArr.Builder[λ[`f[_]` => A], B, Configurable[B]](
      f =>
        new Configurable[B] {
          def apply[F[_]: ConfigMonad](cfg: ConfigItem[F]): F[B] = self(cfg).flatMap(f(_))
        }
    )

  def catching[B](f: A => B)(message: ConfigFunc[(A, Throwable), B]): Configurable[B] =
    flatMake[B] { implicit F => x =>
      try F.monad.pure(f(x))
      catch { case NonFatal(ex) => message((x, ex)) }
    }

  def catchMake[B](f: A => B) =
    ConfigArr.Builder[λ[`f[_]` => (A, Throwable)], B, Configurable[B]](
      g =>
        flatMake[B] { implicit F => x =>
          try F.monad.pure(f(x))
          catch { case NonFatal(ex) => g((x, ex)) }
        }
    )

  def widen[A1 >: A]: Configurable[A1] = this.asInstanceOf[Configurable[A1]]
}

object Configurable extends BaseGetters {

  def apply[A](implicit cfg: Configurable[A]): Configurable[A] = cfg

  def parse[F[_]: ConfigMonad, A](item: ConfigItem[F])(implicit cfg: Configurable[A]): F[A] = cfg(item)

  def make[A] = ConfigArr.Builder[ConfigItem, A, Configurable[A]] { f =>
    new Configurable[A] { def apply[F[_]: ConfigMonad](cfg: ConfigItem[F]): F[A] = f(cfg) }
  }

  type Typeclass[T] = Configurable[T]

  def requireSimple[A](vtype: ValueTypeSimple[A]): Configurable[A] =
    make(F => {
      case vtype(a)            => F.monad.pure(a)
      case item: ConfigItem[_] => F.config.badType(List(vtype), item.valueType)
    })

  def requireOneOf[A, B](vtypeA: ValueTypeSimple[A], vtypeB: ValueTypeSimple[B]): Configurable[Either[A, B]] =
    make(F => {
      case vtypeA(a)           => F.monad.pure(Left(a))
      case vtypeB(b)           => F.monad.pure(Right(b))
      case item: ConfigItem[_] => F.config.badType(List(vtypeA, vtypeB), item.valueType)
    })

  def combine[T](ctx: CaseClass[Configurable, T]): Configurable[T] =
    new Configurable[T] {
      def apply[F[_]](cfg: ConfigItem[F])(implicit F: ConfigMonad[F]): F[T] = cfg match {
        case ValueType.Dict(get, _) =>
          Chain
            .fromSeq(ctx.parameters)
            .parTraverse[F, Any](
              param => get(param.label).flatMap(param.typeclass.apply[F]).local(Prop(param.label) +: _).map(x => x)
            )
            .map(c => ctx.rawConstruct(c.toList))
        case it => ConfigErrors.badType[F, T](ValueType.Dict)(it.valueType)
      }
    }

  def dispatch[T](ctx: SealedTrait[Configurable, T]): Configurable[T] =
    new Typeclass[T] {
      def apply[F[_]](cfg: ConfigItem[F])(implicit F: ConfigMonad[F]): F[T] =
        ctx.subtypes.toList
          .parTraverse[F, Option[(String, T)]](
            sub =>
              sub
                .typeclass[F](cfg)
                .map(x => sub.typeName.short -> (x: T))
                .local(Variant(sub.typeName.short) +: _)
                .restore[F]
          )
          .flatMap(_.unite match {
            case Nil                => F.config.noVariantFound
            case (_, res) :: Nil    => res.pure[F]
            case (name1, _) :: rest => F.config.multipleVariants(NonEmptyList(name1, rest.map(_._1)))
          })

    }

  implicit final def gen[T]: Typeclass[T] = macro Magnolia.gen[T]

}
trait BaseGetters { self: Configurable.type =>
  //simple types
  implicit val booleanConfigurable: Configurable[Boolean] = requireSimple(ValueType.Bool)
  implicit val intConfigurable: Configurable[Int] = requireSimple(ValueType.Num).flatMake[Int] { F => b =>
    if (b.isValidInt) F.monad.pure(b.toIntExact) else F.config.badNumber(b, "bad integer value")
  }
  implicit val longConfigurable: Configurable[Long] = requireSimple(ValueType.Num).flatMake[Long] { F => b =>
    if (b.isValidLong) F.monad.pure(b.toLongExact) else F.config.badNumber(b, "bad long value")
  }
  implicit val stringConfigurable: Configurable[String] = requireSimple(ValueType.Str)
  implicit val doubleConfigurable: Configurable[Double] = requireSimple(ValueType.Num).map(_.toDouble)

  implicit val durationConfigurable: Configurable[Duration] = requireSimple(ValueType.Str).flatMake[Duration] {
    F => s =>
      try F.monad.pure(Duration(s))
      catch { case _: RuntimeException => F.config.badString(s, "bad duration") }
  }
  implicit val finiteDirationConfigurable: Configurable[FiniteDuration] =
    durationConfigurable.flatMake[FiniteDuration](F => {
      case d: FiniteDuration => F.monad.pure(d)
      case d                 => F.config.invalid(s"duration $d is not finite")
    })
  implicit val urlConfigurable: Configurable[URL] =
    requireSimple(ValueType.Str).catchMake(new URL(_))(F => p => F.config.badString(p._1, "bad URL"))

  implicit val uriConfigurable: Configurable[URI] =
    requireSimple(ValueType.Str).catchMake(new URI(_))(F => p => F.config.badString(p._1, "bad URI"))

  //simple sequence types

  private def monoidConfigurable[A: Configurable, C: Monoid](f: A => C): Configurable[C] =
    new Typeclass[C] {
      def apply[F[_]](cfg: ConfigItem[F])(implicit F: ConfigMonad[F]): F[C] = cfg match {
        case ValueType.Array((get, is)) =>
          is.flatMapF(i => get(i).flatMap(parse[F, A](_)).local(Index(i) +: _)).foldMap(f)
        case ValueType.Stream(items) =>
          items.zipWithIndex.flatMapF { case (a, i) => parse[F, A](a).local(Index(i) +: _) }.foldMap(f)
        case it => ConfigErrors.badType[F, C](ValueType.Array, ValueType.Stream)(it.valueType)
      }
    }

  implicit def indexedSeqConfigurable[A: Configurable]: Configurable[IndexedSeq[A]] = vectorConfigurable[A].widen
  implicit def seqConfigurable[A: Configurable]: Configurable[Seq[A]]               = listConfigurable[A].widen
  implicit def vectorConfigurable[A: Configurable]: Configurable[Vector[A]] =
    monoidConfigurable[A, Vector[A]](Vector[A](_))
  implicit def listConfigurable[A: Configurable]: Configurable[List[A]]   = monoidConfigurable[A, List[A]](List[A](_))
  implicit def chainConfigurable[A: Configurable]: Configurable[Chain[A]] = monoidConfigurable[A, Chain[A]](Chain.one)
  implicit def mapConfigurable[A: Configurable]: Configurable[Map[String, A]] =
    new Configurable[Map[String, A]] {
      def apply[F[_]](cfg: ConfigItem[F])(implicit F: ConfigMonad[F]): F[Map[String, A]] = cfg match {
        case ValueType.Dict(get, is) =>
          is.flatMapF(k => get(k).flatMap(parse[F, A](_)).tupleLeft(k))(F.monad, implicitly).foldMapK(Map(_))
        case it => ConfigErrors.badType[F, Map[String, A]](ValueType.Dict)(it.valueType)
      }
    }

  implicit def option[A: Configurable]: Configurable[Option[A]] =
    new Configurable[Option[A]] {
      def apply[F[_]: ConfigMonad](cfg: ConfigItem[F]): F[Option[A]] = cfg match {
        case ConfigItem.Null => none[A].pure[F]
        case item            => parse[F, A](item).map(_.some)
      }
    }
}
