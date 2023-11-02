package tofu.concurrent

import cats.Monad
import cats.data.ReaderT
import cats.effect.unsafe.IORuntime
import cats.effect.{IO, Sync}
import org.scalatest.funsuite.AnyFunSuite
import tofu.syntax.monadic._

import scala.annotation.nowarn

@nowarn
class AtomSuite extends AnyFunSuite {
  implicit val iort: IORuntime = IORuntime.global

  def summonInstance[I[_]: Sync, F[_]: Sync] = {
    implicitly[MakeAtom[I, F]]
  }

  test("check IO has Atom") {
    assert(MakeAtom[IO, ReaderT[IO, Unit, _]].of(1).flatMap(atomProg(_).run(())).unsafeRunSync() === ("1" -> 2))
  }

  def atomProg[F[_]: Monad](atom: Atom[F, Int]) =
    for {
      x <- atom.modify(i => (i + 1, i.toString))
      y <- atom.get
    } yield (x, y)

}
