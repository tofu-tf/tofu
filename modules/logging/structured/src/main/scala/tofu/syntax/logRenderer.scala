package tofu.syntax

import cats.syntax.foldable._
import cats.{Eval, Foldable}
import tofu.logging.{LogParamValue, LogRenderer, Loggable}

import scala.collection.compat.IterableOnce

object logRenderer {
  implicit final class LogRendererValueContextOps[I, V, R, M](private val v: V) extends AnyVal {
    def zero(implicit r: LogRenderer[I, V, R, M]): M                                    = r.zero(v)
    def putLogValue(value: LogParamValue)(implicit r: LogRenderer[I, V, R, M]): M       = r.putValue(value, v)
    def list(size: Int)(receive: (V, Int) => M)(implicit r: LogRenderer[I, V, R, M]): M = r.list(size, v)(receive)

    def foldable[F[_]: Foldable, A](fa: F[A])(receive: (V, A) => M)(implicit r: LogRenderer[I, V, R, M]): M =
      r.foldable(fa, v)(receive)
    def coll[A](fa: IterableOnce[A])(receive: (V, A) => M)(implicit r: LogRenderer[I, V, R, M]): M          =
      r.coll(fa, v)(receive)

    def putFoldable[F[_]: Foldable, A: Loggable](fa: F[A])(implicit r: LogRenderer[I, V, R, M]): M =
      r.putFoldable(fa, v)
    def putColl[A: Loggable](fa: IterableOnce[A])(implicit r: LogRenderer[I, V, R, M]): M          =
      r.putColl(fa, v)

    def dict(receive: I => R)(implicit r: LogRenderer[I, V, R, M]): M = r.dict(v)(receive)

    def whenVal(b: Boolean)(x: => M)(implicit r: LogRenderer[I, V, R, M]): M = if (b) x else r.zero(v)

    def coalsesce(f: V => M)(g: V => M)(implicit r: LogRenderer[I, V, R, M]): M = r.coalesce(f, g, v)

    def foldVal[F[_]: Foldable, A](fa: F[A], v: V)(f: (A, V) => M)(implicit r: LogRenderer[I, V, R, M]): M =
      fa.foldRight[V => M](Eval.later(r.zero))((a, eg) => Eval.defer(eg.map(g => r.coalesce(f(a, _), g, _))))
        .value(v)

    def putString(value: String)(implicit r: LogRenderer[I, V, R, M]): M      = r.putString(value, v)
    def putInt(value: Long)(implicit r: LogRenderer[I, V, R, M]): M           = r.putInt(value, v)
    def putFloat(value: Double)(implicit r: LogRenderer[I, V, R, M]): M       = r.putFloat(value, v)
    def putBigInt(value: BigInt)(implicit r: LogRenderer[I, V, R, M]): M      = r.putBigInt(value, v)
    def putDecimal(value: BigDecimal)(implicit r: LogRenderer[I, V, R, M]): M = r.putDecimal(value, v)
    def putBool(value: Boolean)(implicit r: LogRenderer[I, V, R, M]): M       = r.putBool(value, v)
  }

  implicit final class LogRendererTopContextOps[I, V, R, M](private val i: I) extends AnyVal {
    def noop(implicit r: LogRenderer[I, V, R, M]): R = r.noop(i)

    def sub(name: String)(receive: V => M)(implicit r: LogRenderer[I, V, R, M]): R =
      r.sub(name, i)(receive)

    def whenTop(b: Boolean)(x: => R)(implicit r: LogRenderer[I, V, R, M]): R = if (b) x else r.noop(i)

    def foldTop[F[_]: Foldable, A](fa: F[A])(f: A => R)(implicit r: LogRenderer[I, V, R, M]): R =
      fa.reduceLeftToOption(f)((b, a) => r.combine(b, f(a))).getOrElse(r.noop(i))

    /** add new field to result */
    def addField(name: String, value: LogParamValue)(implicit r: LogRenderer[I, V, R, M]): R =
      r.addField(name, value, i)

    /** focus on subelement of result */
    def subDict(path: String)(receive: I => R)(implicit r: LogRenderer[I, V, R, M]): R = r.subDict(path, i)(receive)

    def field[A: Loggable](path: String, a: A)(implicit r: LogRenderer[I, V, R, M]): R = r.field(path, i, a)

    /** add multiple dicts */
    def subDictList(path: String, size: Int)(receive: (I, Int) => R)(implicit r: LogRenderer[I, V, R, M]): R =
      r.subDictList(path, size, i)(receive)

    def addString(name: String, value: String)(implicit r: LogRenderer[I, V, R, M]): R      =
      r.addString(name, value, i)
    def addInt(name: String, value: Long)(implicit r: LogRenderer[I, V, R, M]): R           =
      r.addInt(name, value, i)
    def addFloat(name: String, value: Double)(implicit r: LogRenderer[I, V, R, M]): R       =
      r.addFloat(name, value, i)
    def addBigInt(name: String, value: BigInt)(implicit r: LogRenderer[I, V, R, M]): R      =
      r.addBigInt(name, value, i)
    def addDecimal(name: String, value: BigDecimal)(implicit r: LogRenderer[I, V, R, M]): R =
      r.addDecimal(name, value, i)
    def addBool(name: String, value: Boolean)(implicit r: LogRenderer[I, V, R, M]): R       =
      r.addBool(name, value, i)
  }
}
