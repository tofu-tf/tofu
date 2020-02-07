package tofu.higherKind.derived
import cats.free.Free
import derevo.derive
import tofu.higherKind.Embed

object EmbedKSuite {
  @derive(embed)
  trait Foo[F[_]] {
    def foo(x: Int, s: String): F[Double]
    def bar(a: List[Int]): Free[F, Unit]
  }

  Embed[Foo]

}
