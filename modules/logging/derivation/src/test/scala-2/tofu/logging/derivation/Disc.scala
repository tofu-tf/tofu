package tofu.logging.derivation
import tofu.compat.unused
import tofu.logging.{LogTree, Loggable}
import tofu.syntax.loggable._

object Disc extends App {

  sealed trait Lol extends Product

  object Lol {
    final case class Kek(a: String, b: Double) extends Lol
    final case class Kukarek(z: Int)           extends Lol
  }

  final case class Discriminated[T](
      `type`: String,
      @unembed value: T
  )

  object Discriminated {
    implicit def lg[T](implicit @unused ev: Loggable[T]): Loggable[Discriminated[T]] = loggable.instance

    def wrap[A <: Product](implicit instance: Loggable[A]): Loggable[A] =
      Loggable[Discriminated[A]].contramap(a => Discriminated(a.productPrefix, a))
  }

  implicit val kekLol: Loggable[Lol] = Discriminated.wrap[Lol](loggable.instance)

  val kek: Lol     = Lol.Kek("ohoho", 1.3)
  val kukarek: Lol = Lol.Kukarek(5)

  println(LogTree(kek))
  println(LogTree(kukarek))

  println(kek.logShow)
  println(kukarek.logShow)
}
