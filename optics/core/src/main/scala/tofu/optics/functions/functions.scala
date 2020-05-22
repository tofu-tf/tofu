package tofu.optics

package object functions extends CollectionFunctions with TupleFunctions with ContainerFunctions {
  def extractSubtype[A <: B, B]: Extract[A, B] = (s: B) => s

  def filter[A](p: A => Boolean): Subset[A, A] = new Subset[A, A] {
    override def narrow(a: A): Either[A, A] = if (p(a)) Right(a) else Left(a)

    override def upcast(b: A): A = b
  }
}
