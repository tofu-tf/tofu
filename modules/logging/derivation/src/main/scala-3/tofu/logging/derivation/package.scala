package tofu.logging.derivation

import magnolia1.TypeInfo
import scala.collection.compat._
import scala.deriving.Mirror
import tofu.logging.Loggable

extension (x: Loggable.type) inline def derived[A](using Mirror.Of[A]): Loggable[A] = loggable.derived[A]

private[derivation] def strJoin(typeName: String, strings: IterableOnce[String]): String =
  if (strings.iterator.isEmpty) typeName else strings.iterator.mkString(s"$typeName{", ",", "}")

private[derivation] def calcTypeName(typeName: TypeInfo, seen: Set[TypeInfo] = Set()): String =
  if (seen(typeName)) "#"
  else {
    val args = typeName.typeParams
    val name = typeName.full

    if (args.isEmpty) name
    else args.iterator.map(calcTypeName(_, seen + typeName)).mkString(name + "[", ",", "]")
  }
