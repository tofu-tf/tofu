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
import cats.syntax.option._
import cats.syntax.parallel._
import cats.syntax.monoid._
import cats.syntax.traverse._
import magnolia.{CaseClass, Magnolia, SealedTrait}
import syntax.context._
import syntax.handle._
import tofu.config.ConfigError.{BadNumber, BadString, BadType, Invalid, MultipleVariants, NoVariantFound}
import tofu.config.ConfigItem._
import tofu.config.Key.{Index, Prop, Variant}
import tofu.syntax.monadic._
import tofu.syntax.raise._

import scala.collection.immutable.IndexedSeq
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.util.control.NonFatal
import tofu.config.MagnoliaDerivation.SingletonConfigurable
import tofu.config.MagnoliaDerivation.EnumConfigurable
import tofu.config.ConfigError.NotFound
import derevo.Derivation

trait Configurable[A] extends ConfigArr[ConfigItem, A] { self =>
  import ConfigMonad.promote._
  def apply[F[_]: ConfigMonad](cfg: ConfigItem[F]): F[A]

  def map[B](f: A => B): Configurable[B]               = new Configurable[B] {
    def apply[F[_]: ConfigMonad](cfg: ConfigItem[F]): F[B] = self(cfg).map(f)
  }
  def flatMap[B](f: ConfigFunc[A, B]): Configurable[B] = new Configurable[B] {
    def apply[F[_]: ConfigMonad](cfg: ConfigItem[F]): F[B] =
      self(cfg).flatMap(f[F](_))
  }

  def flatMake[B] =
    ConfigArr.Builder[λ[`f[_]` => A], B, Configurable[B]](f =>
      new Configurable[B] {
        def apply[F[_]: ConfigMonad](cfg: ConfigItem[F]): F[B] = self(cfg).flatMap(f(_))
      }
    )

  def catching[B](f: A => B)(message: ConfigFunc[(A, Throwable), B]): Configurable[B] =
    flatMake[B] { implicit F => x =>
      try {
        F.monad.pure(f(x))
      } catch { case NonFatal(ex) => message((x, ex)) }
    }

  def catchMake[B](f: A => B) =
    ConfigArr.Builder[λ[`f[_]` => (A, Throwable)], B, Configurable[B]](g =>
      flatMake[B] { implicit F => x =>
        try F.monad.pure(f(x))
        catch { case NonFatal(ex) => g((x, ex)) }
      }
    )

  def widen[A1 >: A]: Configurable[A1] = this.asInstanceOf[Configurable[A1]]
}

object Configurable extends BaseGetters with MagnoliaDerivation {

  def apply[A](implicit cfg: Configurable[A]): Configurable[A] = cfg

  def parse[F[_]: ConfigMonad, A](item: ConfigItem[F])(implicit cfg: Configurable[A]): F[A] = cfg(item)

  def make[A] = ConfigArr.Builder[ConfigItem, A, Configurable[A]] { f =>
    new Configurable[A] { def apply[F[_]: ConfigMonad](cfg: ConfigItem[F]): F[A] = f(cfg) }
  }

  def requireSimple[A](vtype: ValueTypeSimple[A]): Configurable[A] =
    make(F => {
      case vtype(a)            => F.pure(a)
      case Null                => F.error(NotFound)
      case item: ConfigItem[_] => F.error(BadType(List(vtype), item.valueType))
    })

  def requireOneOf[A, B](vtypeA: ValueTypeSimple[A], vtypeB: ValueTypeSimple[B]): Configurable[Either[A, B]] =
    make(F => {
      case vtypeA(a)           => F.pure(Left(a))
      case vtypeB(b)           => F.pure(Right(b))
      case Null                => F.error(NotFound)
      case item: ConfigItem[_] => F.error(BadType(List(vtypeA, vtypeB), item.valueType))
    })
}
trait BaseGetters { self: Configurable.type =>
  //simple types

  private def numConfigurable[X](check: BigDecimal => Boolean, get: BigDecimal => X, name: String): Configurable[X] =
    requireSimple(ValueType.Num).flatMake { F => b =>
      if (check(b)) F.monad.pure(get(b)) else F.error(BadNumber(b, s"bad $name value"))
    }

  implicit val byteConfigurable: Configurable[Byte]     = numConfigurable[Byte](_.isValidByte, _.toByteExact, "byte")
  implicit val shortConfigurable: Configurable[Short]   = numConfigurable[Short](_.isValidShort, _.toShortExact, "short")
  implicit val intConfigurable: Configurable[Int]       = numConfigurable(_.isValidInt, _.toIntExact, "integer")
  implicit val longConfigurable: Configurable[Long]     = numConfigurable(_.isValidLong, _.toLongExact, "integer")
  implicit val floatConfigurable: Configurable[Float]   = requireSimple(ValueType.Num).map(_.toFloat)
  implicit val doubleConfigurable: Configurable[Double] = requireSimple(ValueType.Num).map(_.toDouble)

  implicit val booleanConfigurable: Configurable[Boolean]   = requireSimple(ValueType.Bool)
  implicit val stringConfigurable: Configurable[String]     = requireSimple(ValueType.Str)
  implicit val durationConfigurable: Configurable[Duration] =
    requireSimple(ValueType.Str).catching[Duration](Duration(_))(ConfigFunc.apply(F => { case (s, _) =>
      F.error(BadString(s, "bad duration"))
    }))

  implicit val finiteDirationConfigurable: Configurable[FiniteDuration] =
    durationConfigurable.flatMake[FiniteDuration](F => {
      case d: FiniteDuration => F.pure(d)
      case d                 => F.error(Invalid(s"duration $d is not finite"))
    })
  implicit val urlConfigurable: Configurable[URL]                       =
    requireSimple(ValueType.Str).catchMake(new URL(_))(F => p => F.error(BadString(p._1, "bad URL")))

  implicit val uriConfigurable: Configurable[URI] =
    requireSimple(ValueType.Str).catchMake(new URI(_))(F => p => F.error(BadString(p._1, "bad URI")))

  //simple sequence types

  private def monoidConfigurable[A: Configurable, C: Monoid](f: A => C): Configurable[C] =
    new Typeclass[C] {
      def apply[F[_]](cfg: ConfigItem[F])(implicit F: ConfigMonad[F]): F[C] = {
        cfg match {
          case ValueType.Array((get, is)) =>
            is.foldLeft(F.pure(Monoid.empty[C])) { (fc, i) =>
              (fc, get(i).flatMap(parse[F, A](_)).local(_ :+ Index(i)).map(f)).parMapN(_ |+| _)
            }.flatten
          case ValueType.Stream(items)    =>
            items.zipWithIndex
              .foldLeft(F.pure(Monoid.empty[C])) { case (fc, (x, i)) =>
                (fc, parse[F, A](x).local(_ :+ Index(i)).map(f)).parMapN(_ |+| _)
              }
              .flatten
          case Null                       => Monoid.empty[C].pure[F]
          case it                         => F.error(BadType(List[ValueTag](ValueType.Array, ValueType.Stream), it.valueType))
        }
      }
    }

  implicit def indexedSeqConfigurable[A: Configurable]: Configurable[IndexedSeq[A]] = vectorConfigurable[A].widen
  implicit def seqConfigurable[A: Configurable]: Configurable[Seq[A]]               = listConfigurable[A].widen
  implicit def vectorConfigurable[A: Configurable]: Configurable[Vector[A]]         =
    monoidConfigurable[A, Vector[A]](Vector[A](_))
  implicit def listConfigurable[A: Configurable]: Configurable[List[A]]             = monoidConfigurable[A, List[A]](List[A](_))
  implicit def chainConfigurable[A: Configurable]: Configurable[Chain[A]]           = monoidConfigurable[A, Chain[A]](Chain.one)
  implicit def mapConfigurable[A: Configurable]: Configurable[Map[String, A]]       =
    new Configurable[Map[String, A]] {
      def apply[F[_]](cfg: ConfigItem[F])(implicit F: ConfigMonad[F]): F[Map[String, A]] = cfg match {
        case ValueType.Dict(get, is) =>
          is.flatMapF(k => get(k).flatMap(parse[F, A](_)).tupleLeft(k))(F.monad, implicitly).foldMapK(Map(_))
        case it                      => F.error(BadType(List(ValueType.Dict), it.valueType))
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

trait MagnoliaDerivation extends Derivation[Configurable] {
  type Typeclass[T] = Configurable[T]

  def combine[T](ctx: CaseClass[Configurable, T]): Configurable[T] =
    if (ctx.isObject) new SingletonConfigurable(ctx.typeName.short, ctx.construct(_ => ()))
    else
      new Configurable[T] {
        def apply[F[_]](cfg: ConfigItem[F])(implicit F: ConfigMonad[F]): F[T] =
          cfg match {
            case ValueType.Dict(get, _) =>
              Chain
                .fromSeq(ctx.parameters)
                .parTraverse[F, Any](param =>
                  get(param.label).flatMap(param.typeclass.apply[F]).local(_ :+ Prop(param.label)).map(x => x)
                )
                .map(c => ctx.rawConstruct(c.toList))
            case ConfigItem.Null        => F.error(NotFound)
            case it                     => F.error(BadType(List(ValueType.Dict), it.valueType))
          }
      }

  def dispatch[T](ctx: SealedTrait[Configurable, T]): Configurable[T] =
    ctx.subtypes.toList.map(_.typeclass).traverse[Option, Map[String, T]] {
      case en: EnumConfigurable[_] => Some(en.nameMap)
      case _                       => None
    } match {
      case Some(maps) => new EnumConfigurable(maps.reduceLeft(_ ++ _))
      case None       =>
        new Typeclass[T] {
          def apply[F[_]](cfg: ConfigItem[F])(implicit F: ConfigMonad[F]): F[T] =
            ctx.subtypes.toList
              .parTraverse[F, Option[(String, T)]](sub =>
                sub
                  .typeclass[F](cfg)
                  .map(x => sub.typeName.short -> (x: T))
                  .local(_ :+ Variant(sub.typeName.short))
                  .restore
              )
              .flatMap(_.unite match {
                case Nil                => F.error(NoVariantFound)
                case (_, res) :: Nil    => res.pure[F]
                case (name1, _) :: rest => F.error(MultipleVariants(NonEmptyList(name1, rest.map(_._1))))
              })

        }
    }

  implicit final def instance[T]: Typeclass[T] = macro Magnolia.gen[T]
}

object MagnoliaDerivation {
  class SingletonConfigurable[A](name: String, value: A) extends EnumConfigurable[A](Map(name -> value)) {
    override def apply[F[_]](cfg: ConfigItem[F])(implicit F: ConfigMonad[F]): F[A] =
      cfg match {
        case Str(tag) =>
          if (tag == name) value.pure[F]
          else F.error(ConfigError.BadString(tag, s"expected $name"))
        case t        => F.error(ConfigError.BadType(List(ValueType.Str), t.valueType))
      }
  }

  class EnumConfigurable[A](val nameMap: Map[String, A]) extends Configurable[A] {
    private val names                                                     = nameMap.keys.mkString(",")
    def apply[F[_]](cfg: ConfigItem[F])(implicit F: ConfigMonad[F]): F[A] =
      cfg match {
        case Str(tag) => nameMap.get(tag).orRaise[F](ConfigError.BadString(tag, s"expected one of: $names"))
        case t        => F.error(ConfigError.BadType(List(ValueType.Str), t.valueType))
      }
  }
}
