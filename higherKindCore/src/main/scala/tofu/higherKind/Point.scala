package tofu.higherKind
import cats.{ContravariantMonoidal, MonoidK, ~>}

/** equivalent of UnitK ~> F
  * get F[A] for any given type A
  * useful for empty values or typeconstructors with phantom parameters
  * may look like a type class but is not
  */
trait Point[F[_]] {
  def point[A]: F[A]
}

object Point {
  val unit: Point[UnitK] = new Point[UnitK] {
    def point[A]: UnitK[A] = ()
  }

  def covariant[F[+_]](fn: F[Nothing]) = new Point[F] {
    def point[A]: F[A] = fn
  }

  def contravariant[F[-_]](fn: F[Any]) = new Point[F] {
    def point[A]: F[A] = fn
  }

  def emptyK[F[_]](implicit F: MonoidK[F]): Point[F] = new Point[F] {
    def point[A]: F[A] = F.empty[A]
  }

  def contraMonoidal[F[_]](implicit F: ContravariantMonoidal[F]) = new Point[F] {
    override def point[A]: F[A] = F.trivial
  }

  implicit val pointRepresentable: RepresentableK[Point] = new RepresentableK[Point] {
    def tabulate[F[_]](hom: RepK[Point, *] ~> F): Point[F] = new Point[F] {
      def point[A]: F[A] = hom(RepK[Point](_.point))
    }
  }
}
