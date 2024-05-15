package tofu.logging
package zlogs

import derevo.derive
import tofu.logging.derivation.{loggable, loggingBiMid}
import zio._
import zio.URLayer
import tofu.logging.bi.LoggingBiCompanion
import tofu.higherKind.bi.BiTemplate
import tofu.zioInstances.implicits._
import tofu.zioFunctions
import zio.test.ZIOSpecDefault
import zio.test._
import tofu.higherKind.bi.RepresentableB
import tofu.higherKind
import tofu.higherKind.bi.FunBK

object ZLogBiMidSpec extends ZIOSpecDefault {
  import tofu.logging.zlogs.Lurker.Point
  val cls = s"<${classOf[Lurker[Any]].getName()}>"

  def go[E, A](z: ZIO[Lurker.Dep, E, A]): ZIO[Any, Nothing, (Either[E, A], Vector[String])] =
    (for {
      ref  <- Ref.make(Vector[String]())
      // TODO: remake to true ZIO style
      e    <- z.provide(ZTestLog.layer, Lurker.attachLog, ZLayer.succeed(ref)).either
      logs <- ref.get
    } yield (e, logs))

  override def spec =
    suite("ZLogBiMidSpec")(
      test("simple walk") {
        for {
          (res, logs) <- go(Lurker.DO.move(Point(20, 3)))
        } yield assertTrue(
          logs == Vector(
            s"[Debug] $cls entering move (target = Point{x=20,y=3})",
            s"[Debug] $cls leaving move (target = Point{x=20,y=3}) result is Point{x=5,y=3}",
          ),
          res == Right(Point(5, 3))
        )
      },
      test("attack") {
        for {
          (res, logs) <- go(Lurker.DO.attack(Point(20, 3)))
        } yield assertTrue(
          res == Left(Lurker.Normal),
          logs == Vector(
            s"[Debug] $cls entering attack (target = Point{x=20,y=3})",
            s"[Error] $cls error during attack (target = Point{x=20,y=3}) error is cant do this operation in Normal state",
          )
        )
      },
      test("burrow and attack") {
        for {
          (res, logs) <- go(Lurker.DO.burrowChange *> Lurker.DO.attack(Point(2, 3)))
        } yield assertTrue(
          res == Right(true),
          logs == Vector(
            s"[Debug] $cls entering burrowChange ()",
            s"[Debug] $cls leaving burrowChange () result is ()",
            s"[Debug] $cls entering attack (target = Point{x=2,y=3})",
            s"[Debug] $cls leaving attack (target = Point{x=2,y=3}) result is true",
          )
        )
      }
    )
}

@derive(loggingBiMid)
trait Lurker[F[_, _]] extends BiTemplate[F] {
  import Lurker._
  def move(target: Point): F[State, Point]
  def burrowChange: FS[Unit]
  def attack(target: Point): F[State, Boolean]
}

object Lurker extends LoggingBiCompanion[Lurker] {
  // TODO: use `@derive(representableB)` from derivation module when it is ready
  implicit def reprBInstance: RepresentableB[Lurker] = new RepresentableB[Lurker] {
    override def bitabulate[F[_, _]](repr: FunBK[higherKind.bi.RepBK[Lurker, _, _], F]): Lurker[F] =
      new Lurker[F] {
        def move(target: Point): F[State, Point]     =
          repr(higherKind.bi.RepBK[Lurker](_.move(target)))
        def burrowChange: FS[Unit]                   =
          repr(higherKind.bi.RepBK[Lurker](_.burrowChange))
        def attack(target: Point): F[State, Boolean] =
          repr(higherKind.bi.RepBK[Lurker](_.attack(target)))
      }
  }

  type Dep = Lurker[IO]

  sealed trait State
  case object Normal extends State
  case object Buried extends State
  implicit val wrongStateLoggable: Loggable[State] =
    Loggable[String].contramap(st => s"cant do this operation in $st state")

  lazy val DO: Lurker[ZIO[Dep, +_, +_]] = zioFunctions.expose[Lurker]

  @derive(loggable)
  final case class Point(x: Long, y: Long)

  class Test(ref: Ref.Synchronized[(State, Point)], maxDistance: Long = 5) extends Lurker[IO] {

    private def moveOne(cur: Long, target: Long) = {
      val shift = target - cur
      val dist  = shift.abs.min(maxDistance)
      if (shift >= 0) cur + dist else cur - dist
    }
    def move(target: Point): IO[State, Point]    =
      ref.modifyZIO {
        case (Normal, pt) =>
          val res = Point(moveOne(pt.x, target.x), moveOne(pt.y, target.y))
          ZIO.succeed((res, (Normal, res)))
        case (Buried, _)  => ZIO.fail(Buried)
      }

    def burrowChange: IO[Nothing, Unit] = ref.update {
      case (Normal, pt) => (Buried, pt)
      case (Buried, pt) => (Normal, pt)
    }

    def attack(target: Point): IO[State, Boolean] = ref.get.flatMap {
      case (Normal, _)  =>
        ZIO.fail(Normal)
      case (Buried, pt) =>
        ZIO.succeed((pt.x - target.x).abs < maxDistance && (pt.y - target.x).abs < maxDistance)
    }
  }

  val make: UIO[Lurker[IO]] = Ref.Synchronized.make((Normal: State, Point(0, 0))).map(new Test(_))

  val attachLog: URLayer[ZLogging.Make, Dep] =
    ZLayer.fromZIO(ZIO.serviceWithZIO[ZLogging.Make](implicit logs => make.map(_.attachLogs)))
}
