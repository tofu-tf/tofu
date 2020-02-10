package tofu.higherKind.derived
import cats.data.{IorT, NonEmptyChain}
import cats.free.Free
import derevo.derive
import tofu.higherKind.Embed

object EmbedKSuite {
  @derive(embed)
  trait Foo[F[_]] {
    def foo(x: Int, s: String): F[Double]
    def bar(a: List[Int]): Free[F, Unit]
    def baz(xx: Double): IorT[F, NonEmptyChain[String], Long]
  }

  Embed[Foo]

}
