package tofu.doobie.log

trait LoggableSql {
  implicit def toLoggableSqlInterpolator(ctx: StringContext): LoggableSqlInterpolator =
    new LoggableSqlInterpolator(ctx)
}
