package tofu.concurrent

import cats.effect.Sync
import cats.effect.concurrent.Ref
import tofu.concurrent.ce2._

/** Effectful making and initialization for `Ref`. A `Ref` instance will be initialized in the `I[_]` effect. `F[_]` is
  * the effect in which 'Ref' will work.
  */
trait MakeRef[I[_], F[_]] {
  def refOf[A](a: A): I[Ref[F, A]]
}

object Refs {

  /** Creates a `MakeRef[F,F]` when both effect constructors are the same.
    *
    * @param agents
    *   an given instance of `Refs[F]` (type alias for `MakeRef[F, F]`)
    * @return
    *   instance of `Applier[F, F]`
    */
  def apply[F[_]](implicit agents: Refs[F]): MakeRef.Applier[F, F] = new MakeRef.Applier[F, F](agents)
}

object MakeRef {

  /** Makes a `Ref` instance
    *
    * Uses the [[https://typelevel.org/cats/guidelines.html#partially-applied-type-params Partially-Applied Type]]
    * technique.
    *
    * {{{
    *   MakeRef[I,F].of(10) <-> MakeRef[I,F].of[Int](10)
    * }}}
    *
    * @param makeRef
    *   a given instance of `MakeRef[I,F]`
    * @return
    *   instance of [[Applier]]
    */
  def apply[I[_], F[_]](implicit makeRef: MakeRef[I, F]) = new Applier[I, F](makeRef)

  final class Applier[I[_], F[_]](private val makeRef: MakeRef[I, F]) extends AnyVal {

    /** Makes a Ref initialized to the supplied value. Like [[Ref.of]] but initializes state using another effect
      * constructor.
      *
      * @see
      *   [[MakeRef.refOf]]
      * @return
      *   instance of [[Ref]] initialized to the supplied value.
      */
    def of[A](a: A): I[Ref[F, A]] = makeRef.refOf(a)
  }

  /** Give an instance of `MakeRef[I, F]` to making `Ref` for effect with `Sync` type class.
    *
    * @return
    *   instance of `MakeRef[I,F]`
    */
  implicit def syncInstance[I[_]: Sync, F[_]: Sync]: MakeRef[I, F] = new MakeRef[I, F] {
    def refOf[A](a: A): I[Ref[F, A]] = Ref.in[I, F, A](a)
  }
}
