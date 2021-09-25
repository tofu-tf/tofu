package tofu.logging
package zlogs

import derevo.derive
import org.scalatest.funsuite.AnyFunSuite
import tofu.logging.derivation.{loggable, loggingBiMid}
import zio.{Has, IO, RefM, ZIO, ZLayer}
import zio.URLayer
import tofu.logging.bi.LoggingBiCompanion
import tofu.higherKind.derived.representableB
import tofu.higherKind.bi.BiTemplate
import tofu.zioInstances.implicits._
import zio.UIO
import zio.ZRef
import tofu.zioFunctions

class ZLogBiMidSuite extends AnyFunSuite {
  import tofu.logging.zlogs.Lurker.{Point}
  val cls                                                                = s"<${classOf[Lurker[Any]].getName()}>"
  def go[E, A](z: ZIO[Lurker.Dep, E, A]): (Either[E, A], Vector[String]) =
    zio.Runtime.default.unsafeRun((for {
      ref  <- ZRef.make(Vector[String]())
      e    <- z.provideLayer(ZTestLog.test >>> Lurker.attachLog).provide(ref).either
      logs <- ref.get
    } yield (e, logs)))

  test("simple walk") {
    val (res, logs) = go(Lurker.DO.move(Point(20, 3)))
    assert(res === Right(Point(5, 3)))
    assert(
      logs === Vector(
        s"[Debug] $cls entering move (target = Point{x=20,y=3})",
        s"[Debug] $cls leaving move (target = Point{x=20,y=3}) result is Point{x=5,y=3}",
      )
    )
  }

  test("attack") {
    val (res, logs) = go(Lurker.DO.attack(Point(20, 3)))
    assert(res === Left(Lurker.Normal))
    assert(
      logs === Vector(
        s"[Debug] $cls entering attack (target = Point{x=20,y=3})",
        s"[Error] $cls error during attack (target = Point{x=20,y=3}) error is cant do this operation in Normal state",
      )
    )
  }

  test("burrow and attack") {
    val (res, logs) = go(Lurker.DO.burrowChange *> Lurker.DO.attack(Point(2, 3)))
    assert(res === Right(true))
    assert(
      logs === Vector(
        s"[Debug] $cls entering burrowChange ()",
        s"[Debug] $cls leaving burrowChange () result is ()",
        s"[Debug] $cls entering attack (target = Point{x=2,y=3})",
        s"[Debug] $cls leaving attack (target = Point{x=2,y=3}) result is true",
      )
    )
  }

}

@derive(loggingBiMid, representableB)
trait Lurker[F[_, _]] extends BiTemplate[F] {
  import Lurker._
  def move(target: Point): F[State, Point]
  def burrowChange: FS[Unit]
  def attack(target: Point): F[State, Boolean]
}

object Lurker extends LoggingBiCompanion[Lurker] {
  type Dep = Has[Lurker[IO]]

  sealed trait State
  case object Normal extends State
  case object Buried extends State
  implicit val wrongStateLoggable: Loggable[State] =
    Loggable[String].contramap(st => s"cant do this operation in $st state")

  lazy val DO: Lurker[ZIO[Dep, +*, +*]]            = zioFunctions.expose[Lurker]

  @derive(loggable)
  final case class Point(x: Long, y: Long)

  class Test(ref: RefM[(State, Point)], maxDistance: Long = 5) extends Lurker[IO] {

    private def moveOne(cur: Long, target: Long) = {
      val shift = target - cur
      val dist  = shift.abs.min(maxDistance)
      if (shift >= 0) cur + dist else cur - dist
    }
    def move(target: Point): IO[State, Point]    =
      ref.modify {
        case (Normal, pt) =>
          val res = Point(moveOne(pt.x, target.x), moveOne(pt.y, target.y))
          ZIO.succeed((res, (Normal, res)))
        case (Buried, _)  => ZIO.fail(Buried)
      }

    def burrowChange: IO[Nothing, Unit] = ref.update {
      case (Normal, pt) => ZIO.succeed((Buried, pt))
      case (Buried, pt) => ZIO.succeed((Normal, pt))
    }

    def attack(target: Point): IO[State, Boolean] = ref.get.flatMap {
      case (Normal, _)  =>
        ZIO.fail(Normal)
      case (Buried, pt) =>
        ZIO.succeed((pt.x - target.x).abs < maxDistance && (pt.y - target.x).abs < maxDistance)
    }
  }

  val test: UIO[Lurker[IO]] = RefM.make((Normal: State, Point(0, 0))).map(new Test(_))

  val attachLog: URLayer[TofuLogs, Dep] =
    ZLayer.fromServiceM(implicit logs => test.map(_.attachLogs))
}
