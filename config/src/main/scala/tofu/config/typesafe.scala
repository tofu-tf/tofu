package tofu
package config

import ConfigItem._
import cats.Id
import com.typesafe.config.{Config, ConfigValue}

import scala.collection.JavaConverters._

object typesafe {
  def fromConfig(cfg: Config): ConfigItem[Id] =
    fromValue(cfg.root())

  def fromValue(cfg: ConfigValue): ConfigItem[Id] =
    if (cfg == null) Null else fromUnwrapped(cfg.unwrapped())

  def fromUnwrapped(el: Any): ConfigItem[Id] =
    el match {
      case null                                   => Null
      case b: java.lang.Byte                      => Num(BigDecimal(b.intValue()))
      case s: java.lang.Short                     => Num(BigDecimal(s.intValue()))
      case i: Integer                             => Num(BigDecimal(i))
      case l: java.lang.Long                      => Num(BigDecimal(l))
      case bi: java.math.BigInteger               => Num(BigDecimal(bi))
      case bd: java.math.BigDecimal               => Num(BigDecimal(bd))
      case n: Number                              => Num(BigDecimal(n.doubleValue()))
      case b: java.lang.Boolean                   => Bool(b)
      case s: String                              => Str(s)
      case l: java.util.List[_]                   => seq(l.asScala.toVector.map(fromUnwrapped))
      case m: java.util.Map[String @unchecked, _] => dict(m.asScala.mapValues(fromUnwrapped).toMap)
    }
}
