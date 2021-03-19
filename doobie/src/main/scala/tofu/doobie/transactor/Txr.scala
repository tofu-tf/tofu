package tofu.doobie
package transactor

import cats.data.ReaderT
import cats.{Monad, ~>}
import doobie.{ConnectionIO, Transactor}
import fs2.Stream
import tofu.lift.Lift
import tofu.syntax.funk._
import tofu.{BracketThrow, HasContext}

/** A simple facade for [[doobie.Transactor]] that holds an inner database effect type `DB[_]` and provides
  * natural transformations from this effect to the target effect `F[_]`.
  *
  * The motivation for using this facade instead of `Transactor` is to:
  * 1) initialize all its natural transformations early and remove the need for additional constraints
  *    on `F[_]` later (e.g. `Bracket`);
  * 2) be able to use another `DB[_]` effect besides `ConnectionIO` and build a layer of transactional logic
  *    with it.
  *
  * N.B. The only extended DB effect provided below is `ConnectionRIO`. There are intentionally no effects
  * based on `EitherT` to provide a separate error channel for business-errors. This is due to the (bad) design
  * of Cats Effect. Doobie uses `Throwable`-bounded [[cats.effect.Resource]] and [[fs2.Stream]] under the hood
  * for applying a [[doobie.util.transactor.Strategy]], and throwing an error through `EitherT`'s left channel
  * does not lead to the proper `ExitCase` of a bracket. All errors on the DB layer must be thrown and handled
  * via the `Throwable` channel of `ConnectionIO`.
  */
trait Txr[F[_]] {
  type DB[_]

  /** Interprets `DB` into `F`, applying the transactional strategy. */
  def trans: DB ~> F

  /** Interprets `DB` into `F`, no strategy applied. */
  def rawTrans: DB ~> F

  /** Translates the stream, applying the transactional strategy. */
  def transP: Stream[DB, *] ~> Stream[F, *]

  /** Translates the stream, no strategy applied. */
  def rawTransP: Stream[DB, *] ~> Stream[F, *]
}

object Txr {
  type Aux[F[_], DB0[_]]     = Txr[F] { type DB[x] = DB0[x] }
  type Plain[F[_]]           = Aux[F, ConnectionIO]
  type Lifted[F[_]]          = Aux[F, ConnectionIO]
  type Contextual[F[_], Ctx] = Aux[F, ConnectionRIO[Ctx, *]]

  /** Creates a plain facade that preserves the effect of `Transactor` with `ConnectionIO` as the database effect.
    */
  def plain[F[_]: BracketThrow](t: Transactor[F]): Txr.Plain[F] =
    new Txr[F] {
      type DB[x] = ConnectionIO[x]

      def trans: ConnectionIO ~> F    = t.trans
      def rawTrans: ConnectionIO ~> F = t.rawTrans

      def transP: Stream[ConnectionIO, *] ~> Stream[F, *]    = t.transP
      def rawTransP: Stream[ConnectionIO, *] ~> Stream[F, *] = t.rawTransP
    }

  /** Creates a facade that lifts the effect of `Transactor` from `F[_]` to `G[_]` with `ConnectionIO` as the database
    * effect.
    */
  def lifted[G[_]]: LiftedPA[G] = new LiftedPA[G]

  private[transactor] final class LiftedPA[G[_]](private val dummy: Boolean = true) extends AnyVal {
    def apply[F[_]: BracketThrow](t: Transactor[F])(implicit L: Lift[F, G]): Txr.Lifted[G] =
      new Txr[G] {
        type DB[x] = ConnectionIO[x]

        def trans: ConnectionIO ~> G    = liftTrans(t.trans)
        def rawTrans: ConnectionIO ~> G = liftTrans(t.rawTrans)

        private def liftTrans(fk: ConnectionIO ~> F): ConnectionIO ~> G = fk andThen L.liftF

        def transP: Stream[ConnectionIO, *] ~> Stream[G, *]    = liftTransP(t.transP)
        def rawTransP: Stream[ConnectionIO, *] ~> Stream[G, *] = liftTransP(t.rawTransP)

        private def liftTransP(fk: Stream[ConnectionIO, *] ~> Stream[F, *]): Stream[ConnectionIO, *] ~> Stream[G, *] =
          funK(s => fk(s).translate(L.liftF))
      }
  }

  /** Creates a contextual facade that lifts the effect of `Transactor` from `F[_]` to `G[_]` given `G HasContext R`
    * with `ConnectionRIO[R, *]` as the database effect.
    */
  def contextual[G[_]]: ContextualPA[G] = new ContextualPA[G]

  private[transactor] final class ContextualPA[G[_]](private val dummy: Boolean = true) extends AnyVal {
    def apply[F[_]: BracketThrow, R](
        t: Transactor[F]
    )(implicit L: Lift[F, G], F: Monad[G], C: G HasContext R): Txr.Contextual[G, R] =
      new Txr[G] {
        type DB[x] = ConnectionRIO[R, x]

        def trans: ConnectionRIO[R, *] ~> G    = liftTrans(t.trans)
        def rawTrans: ConnectionRIO[R, *] ~> G = liftTrans(t.rawTrans)

        private def liftTrans(fk: ConnectionIO ~> F): ConnectionRIO[R, *] ~> G =
          funK(crio => C.askF(ctx => L.lift(fk(crio.run(ctx)))))

        def transP: Stream[ConnectionRIO[R, *], *] ~> Stream[G, *]    = liftTransPK(t.transPK)
        def rawTransP: Stream[ConnectionRIO[R, *], *] ~> Stream[G, *] = liftTransPK(t.rawTransPK)

        private def liftTransPK(
            fk: Stream[ConnectionRIO[R, *], *] ~> Stream[ReaderT[F, R, *], *]
        ): Stream[ConnectionRIO[R, *], *] ~> Stream[G, *] =
          funK(s => fk(s).translate(funKFrom[ReaderT[F, R, *]](rio => C.askF(ctx => L.lift(rio.run(ctx))))))
      }
  }

}
