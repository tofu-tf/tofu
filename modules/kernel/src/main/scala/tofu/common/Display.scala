package tofu.common
import cats.{Eval, Show}
import java.util.UUID
import scala.collection.immutable.{BitSet, Queue, Seq, SortedMap, SortedSet}
import scala.concurrent.duration.Duration
import scala.util.Try

/** Configurable and performant conversion to String */
trait Display[A] extends Show[A] {

  /** Represents value as a collection of parts from which it can be shown.
    *
    * @note
    *   Newlines are managed solely by instances of [[Display]].
    */
  def displayBuild(cfg: Display.Config, a: A): Eval[Vector[String]]

  def display(a: A, config: Display.Config): String =
    displayBuild(config, a).value.mkString

  def show(a: A): String = display(a, Display.Config.default)
}

object Display extends DisplaySyntax with DisplayInstances {
  def apply[A: Display]: Display[A] = implicitly

  final case class Config(
      fieldSeparator: String = ",",
      indent: String = "\t",
      showFieldLabels: Boolean = true,
      brackets: Brackets = Brackets.curly,
      fieldAssign: String = " = ",
      newline: String = "\n",
      quote: String = "\""
  )

  object Config {
    val default: Config = Config()
  }

  final case class Brackets(left: String, right: String)

  object Brackets {
    val curly: Brackets  = Brackets("{", "}")
    val square: Brackets = Brackets("[", "]")
    val round: Brackets  = Brackets("(", ")")
  }

}

trait DisplaySyntax {
  implicit class displayOps[A: Display](a: A) {
    def display(config: Display.Config = Display.Config.default): String = Display[A].display(a, config)
  }

}
trait DisplayInstances {
  def fromShow[A: Show]: Display[A] = (_: Display.Config, a: A) => Eval.now(Vector(Show[A].show(a)))

  implicit lazy val stringDisplay: Display[String]       = (cfg: Display.Config, a: String) =>
    Eval.now(Vector(cfg.quote + a + cfg.quote))
  implicit def displayForUnit: Display[Unit]             = fromShow(cats.instances.unit.catsStdShowForUnit)
  implicit def displayForBoolean: Display[Boolean]       = fromShow(cats.instances.boolean.catsStdShowForBoolean)
  implicit def displayForByte: Display[Byte]             = fromShow(cats.instances.byte.catsStdShowForByte)
  implicit def displayForShort: Display[Short]           = fromShow(cats.instances.short.catsStdShowForShort)
  implicit def displayForInt: Display[Int]               = fromShow(cats.instances.int.catsStdShowForInt)
  implicit def displayForLong: Display[Long]             = fromShow(cats.instances.long.catsStdShowForLong)
  implicit def displayForFloat: Display[Float]           = fromShow(cats.instances.float.catsStdShowForFloat)
  implicit def displayForDouble: Display[Double]         = fromShow(cats.instances.double.catsStdShowForDouble)
  implicit def displayForBigInt: Display[BigInt]         = fromShow(cats.instances.bigInt.catsStdShowForBigInt)
  implicit def displayForBigDecimal: Display[BigDecimal] = fromShow(cats.instances.bigDecimal.catsStdShowForBigDecimal)
  implicit def displayForChar: Display[Char]             = fromShow(cats.instances.char.catsStdShowForChar)
  implicit def displayForSymbol: Display[Symbol]         = fromShow(cats.instances.symbol.catsStdShowForSymbol)
  implicit def displayForUUID: Display[UUID]             = fromShow(cats.instances.uuid.catsStdShowForUUID)
  implicit def displayForDuration: Display[Duration]     = fromShow(
    cats.instances.duration.catsStdShowForDurationUnambiguous
  )
  implicit def displayForBitSet: Display[BitSet]         = fromShow(cats.instances.bitSet.catsStdShowForBitSet)

  implicit def displayForOption[A: Display]: Display[Option[A]]                      = fromShow(
    cats.instances.option.catsStdShowForOption[A]
  )
  implicit def displayForTry[A: Display]: Display[Try[A]]                            = fromShow(cats.instances.try_.catsStdShowForTry[A])
  implicit def displayForList[A: Display]: Display[List[A]]                          = fromShow(cats.instances.list.catsStdShowForList[A])
  implicit def displayForSeq[A: Display]: Display[Seq[A]]                            = fromShow(cats.instances.seq.catsStdShowForSeq[A])
  implicit def displayForVector[A: Display]: Display[Vector[A]]                      = fromShow(
    cats.instances.vector.catsStdShowForVector[A]
  )
  implicit def displayForQueue[A: Display]: Display[Queue[A]]                        = fromShow(cats.instances.queue.catsStdShowForQueue[A])
  implicit def displayForEither[A: Display, B: Show]: Show[Either[A, B]]             =
    cats.instances.either.catsStdShowForEither[A, B]
  implicit def displayForSet[A: Display]: Display[Set[A]]                            = fromShow(cats.instances.set.catsStdShowForSet[A])
  implicit def displayForMap[K: Display, V: Display]: Display[Map[K, V]]             = fromShow(
    cats.instances.map.catsStdShowForMap[K, V]
  )
  implicit def displayForSortedSet[A: Display]: Display[SortedSet[A]]                = fromShow(
    cats.instances.sortedSet.catsStdShowForSortedSet[A]
  )
  implicit def displayForSortedMap[K: Display, V: Display]: Display[SortedMap[K, V]] =
    fromShow(cats.instances.sortedMap.catsStdShowForSortedMap[K, V])

}
