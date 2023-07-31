package tofu.internal.carriers

import scala.concurrent.Future

class Kek[F[_]]

object abc {

  def testPath[F[_]]: Kek[F] = new Kek[F] {
    override def toString(): String = "KEKE HAHA"
  }
}