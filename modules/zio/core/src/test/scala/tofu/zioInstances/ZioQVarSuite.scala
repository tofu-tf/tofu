package tofu.zioInstances

import cats.syntax.parallel._
import cats.{Monad, Parallel}
import org.scalatest.funsuite.AnyFunSuite
import tofu.concurrent.{Atom, MakeAtom, QVar, QVars}
import tofu.generate.GenRandom
import tofu.syntax.monadic._
import tofu.syntax.time._
import tofu.time.Sleep
import zio.{URIO, ZIO}

class ZioQVarSuite extends AnyFunSuite {
  class Operations[F[_]: Monad: Sleep: GenRandom](
      qvar: QVar[F, Int],
      ref: Atom[F, Int],
      errs: Atom[F, Int]
  ) {
    import scala.concurrent.duration._

    val error = errs.update(_ + 1)

    val randomSleep = GenRandom.nextInt(10).map(_.millis).flatMap(sleep[F])

    val updater: F[Unit] = for {
      _ <- randomSleep
      x <- qvar.take
      y <- ref.modify(x => (-1, x))
      _ <- randomSleep
      _ <- ref.set(y + 1)
      _ <- qvar.put(x + 1)
    } yield ()

    val checker: F[Unit] = for {
      _ <- randomSleep
      a <- qvar.take
      b <- ref.get
      _ <- qvar.put(a)
      _ <- error.whenA(a != b)
    } yield ()

    def observer(prev: Int, remains: Int): F[(Int, Int)] = for {
      _ <- randomSleep
      a <- qvar.read
      _ <- error.whenA(a < prev)
    } yield (a, remains - 1)

    val result = errs.get
  }

  def program[F[_]: Monad: Parallel](ops: Operations[F]): F[Int] = {
    val opss = List.fill(20)(ops)
    opss.parTraverse_(_.updater.replicateM_(10)) &>
      opss.parTraverse(o => (0, 20).iterateWhileM((o.observer _).tupled)(_._2 >= 0)) &>
      opss.parTraverse_(_.checker.replicateM_(20)) >>
      ops.result

  }

  type Z[+A] = URIO[zio.ZEnv, A]

  test("QVar works kinda fine") {
    import zio.interop.catz._
    import tofu.zioInstances.implicits._
    import zio.duration._
    val res = zio.Runtime.default.unsafeRun(
      ZIO
        .mapN(
          QVars[Z].of(1),
          MakeAtom[Z, Z].of(1),
          MakeAtom[Z, Z].of(0)
        )(new Operations(_, _, _))
        .flatMap(program(_))
        .timeout(10.seconds)
    )

    assert(res === Some(0))
  }

}
