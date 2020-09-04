package tofu
package config
import cats.{Order, Show}

sealed trait Key
object Key {
  final case class Index(i: Int)         extends Key
  final case class Prop(name: String)    extends Key
  final case class Variant(name: String) extends Key

  implicit val show: Show[Key] = {
    case Index(i)      => s"[$i]"
    case Prop(name)    => s"$name"
    case Variant(name) => s"#$name#"
  }

  implicit val order: Order[Key] = data.instances.order.derive

}
