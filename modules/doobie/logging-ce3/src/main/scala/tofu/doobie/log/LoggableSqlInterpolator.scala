package tofu.doobie.log
import scala.{specialized => sp}

import doobie.syntax.SqlInterpolator
import doobie.syntax.SqlInterpolator.SingleFragment
import doobie.util.Put
import doobie.util.fragment.Fragment
import doobie.util.pos.Pos
import tofu.doobie.log.LoggableSqlInterpolator.LoggableSingleFragment
import tofu.logging.{LogRenderer, Loggable, LoggedValue}

final class LoggableSqlInterpolator(private val ctx: StringContext) extends AnyVal {
  def lsql(parts: LoggableSingleFragment*)(implicit pos: Pos): Fragment =
    new SqlInterpolator(ctx).sql(parts.map(_.toSingleFragment): _*)
  def lfr(parts: LoggableSingleFragment*)(implicit pos: Pos): Fragment  =
    new SqlInterpolator(ctx).fr(parts.map(_.toSingleFragment): _*)
  def lfr0(parts: LoggableSingleFragment*)(implicit pos: Pos): Fragment =
    new SqlInterpolator(ctx).fr0(parts.map(_.toSingleFragment): _*)
}

object LoggableSqlInterpolator {
  final case class LoggableArg[A](value: A)(implicit val log: Loggable[A]) extends LoggedValue {
    def logFields[I, V, @sp(Unit) R, @sp M](input: I)(implicit r: LogRenderer[I, V, R, M]): R = log.fields(value, input)
    override def putValue[I, V, R, S](v: V)(implicit r: LogRenderer[I, V, R, S]): S           = log.putValue(value, v)
    override def toString: String                                                             = log.logShow(value)
  }
  object LoggableArg {
    implicit def put[A: Put]: Put[LoggableArg[A]] = Put[A].contramap(_.value)
  }

  final case class LoggableSingleFragment(fr: Fragment) extends AnyVal {
    def toSingleFragment: SingleFragment[Nothing] = SingleFragment(fr)
  }
  object LoggableSingleFragment {
    def of[A](sf: SingleFragment[A]): LoggableSingleFragment = new LoggableSingleFragment(sf.fr)

    implicit def fromPut[A: Loggable: Put](a: A): LoggableSingleFragment                =
      LoggableSingleFragment.of(LoggableArg(a))
    implicit def fromPutOption[A: Loggable: Put](oa: Option[A]): LoggableSingleFragment =
      LoggableSingleFragment.of(oa.map(LoggableArg(_)))
    implicit def fromFragment(fr: Fragment): LoggableSingleFragment                     =
      LoggableSingleFragment(fr)
  }
}
