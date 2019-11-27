package tofu.logging

import magnolia.TypeName

package object derivation {
  private[derivation] def join(typeName: String, strings: TraversableOnce[String]): String =
    if (strings.isEmpty) typeName else strings.mkString(s"$typeName{", ",", "}")

  private[derivation] def calcTypeName(typeName: TypeName, seen: Set[TypeName] = Set()): String =
    if (seen(typeName)) "#"
    else {
      val args = typeName.typeArguments
      val name = typeName.full

      if (args.isEmpty) name
      else args.iterator.map(calcTypeName(_, seen + typeName)).mkString(name + "[", ",", "]")
    }
}
