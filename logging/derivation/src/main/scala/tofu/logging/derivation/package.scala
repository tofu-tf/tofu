package tofu.logging

import magnolia.TypeName
import scala.collection.compat._

package object derivation {
  private[derivation] def join(typeName: String, strings: IterableOnce[String]): String =
    if (strings.iterator.isEmpty) typeName else strings.iterator.mkString(s"$typeName{", ",", "}")

  private[derivation] def calcTypeName(typeName: TypeName, seen: Set[TypeName] = Set()): String =
    if (seen(typeName)) "#"
    else {
      val args = typeName.typeArguments
      val name = typeName.full

      if (args.isEmpty) name
      else args.iterator.map(calcTypeName(_, seen + typeName)).mkString(name + "[", ",", "]")
    }
}
