package tofu.internal.carriers

import scala.concurrent.Future

class Kek[F[_]]

class Kek1[F[_], E]

class Kek2[F[_], I[_]]

object abc {

  def testPath[F[_]]: Kek[F] = new Kek[F] {
    override def toString(): String = "KEKE HAHA"
  }

  def testPath1[F[_], E]: Kek1[F, E] = new Kek1[F, E] {
    override def toString(): String = "KEKE HAHA 1"
  }

  def testPath2[F[_], I[_]]: Kek2[F, I] = new Kek2[F, I] {
    override def toString(): String = "KEKE HAHA 2"
  }
}