package tofu.doobie
package transactor

import cats.data.{Kleisli, ReaderT}
import cats.effect.Resource
import cats.{Defer, Monad, ~>}
import doobie.{ConnectionIO, Transactor}
import fs2.Stream
import tofu.lift.Lift
import tofu.syntax.funk._
import tofu.syntax.monadic._
import tofu.{BracketThrow, WithContext}

import scala.annotation.nowarn

/** A simple facade for [[doobie.Transactor]] that holds an inner database effect type `DB[_]` and provides
  * natural transformations from this effect to the target effect `F[_]`.
  *
  * The motivation for using this facade instead of `Transactor` is to:
  *
  *   - initialize all its natural transformations early and remove the need for additional constraints
  *     on `F[_]` later (e.g. `Bracket`);
  *
  *   - be able to use another `DB[_]` effect besides `ConnectionIO` and build a layer of transactional logic
  *     with it.
  */
trait Txr[F[_], DB0[_]] {
  type DB[x] >: DB0[x] <: DB0[x]

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
  type Plain[F[_]]          = Txr[F, ConnectionIO]
  type Continuational[F[_]] = Txr[F, ConnectionCIO[F, *]]

  def apply[F[_], DB[_]](implicit ev: Txr[F, DB]): Txr[F, DB]                 = ev
  def Plain[F[_]](implicit ev: Plain[F]): Plain[F]                            = ev
  def Continuational[F[_]](implicit ev: Continuational[F]): Continuational[F] = ev

  /** Creates a plain facade that preserves the effect of `Transactor` with `ConnectionIO` as the database effect.
    */
  def plain[F[_]: BracketThrow](t: Transactor[F]): Txr.Plain[F] =
    new Txr.Plain[F] {
      val trans: ConnectionIO ~> F    = t.trans
      val rawTrans: ConnectionIO ~> F = t.rawTrans

      val transP: Stream[ConnectionIO, *] ~> Stream[F, *]    = t.transP
      val rawTransP: Stream[ConnectionIO, *] ~> Stream[F, *] = t.rawTransP
    }

  /** Creates a facade that uses `ConnectionCIO` as the database effect.
    */
  def continuational[F[_]: BracketThrow: Defer](t: Transactor[F]): Txr.Continuational[F] =
    new Txr.Continuational[F] {
      val trans: ConnectionCIO[F, *] ~> F    = makeTrans(true)
      val rawTrans: ConnectionCIO[F, *] ~> F = makeTrans(false)

      val transP: Stream[ConnectionCIO[F, *], *] ~> Stream[F, *]    = makeTransP(true)
      val rawTransP: Stream[ConnectionCIO[F, *], *] ~> Stream[F, *] = makeTransP(false)

      private def interpret(withStrategy: Boolean): Resource[F, ConnectionCIO.Cont[F]] = for {
        c <- t.connect(t.kernel)
        f  = new ConnectionCIO.Cont[F] {
               def apply[A](ca: ConnectionIO[A]): F[A] = ca.foldMap(t.interpret).run(c)
             }
        _ <- withStrategy.when_(t.strategy.resource.mapK(f))
      } yield f

      private def makeTrans(withStrategy: Boolean): ConnectionCIO[F, *] ~> F =
        funK(ccio => interpret(withStrategy).use(ccio.run))

      private def makeTransP(withStrategy: Boolean): Stream[ConnectionCIO[F, *], *] ~> Stream[F, *] =
        funK(s =>
          Stream
            .resource(interpret(withStrategy))
            .flatMap(fk => s.translate(Kleisli.applyK[F, ConnectionCIO.Cont[F]](fk)))
        )
    }

  @deprecated("Use `Txr[F, DB]` instead", since = "0.10.3")
  type Aux[F[_], DB[_]]      = Txr[F, DB]
  @deprecated("Use `Transactor.mapK` and `Txr.Plain[F]` instead", since = "0.10.3")
  type Lifted[F[_]]          = Txr[F, ConnectionIO]
  @deprecated("Use `Transactor.mapK` and `Txr.Continuational[F]` instead", since = "0.10.3")
  type Contextual[F[_], Ctx] = Txr[F, ConnectionRIO[Ctx, *]]

  @deprecated("Use `Txr[F, DB]` instead", since = "0.10.3")
  def Aux[F[_], DB[_]](implicit ev: Aux[F, DB]): Aux[F, DB]                      = ev
  @deprecated("Use `Transactor.mapK` and `Txr.plain` instead", since = "0.10.3")
  def Lifted[F[_]](implicit ev: Lifted[F]): Lifted[F]                            = ev
  @deprecated("Use `Transactor.mapK` and `Txr.continuational` as a better alternative", since = "0.10.3")
  def Contextual[F[_], Ctx](implicit ev: Contextual[F, Ctx]): Contextual[F, Ctx] = ev

  /** Creates a facade that lifts the effect of `Transactor` from `F[_]` to `G[_]` with `ConnectionIO` as the database
    * effect.
    */
  @deprecated("Use `Transactor.mapK` and `Txr.plain` instead", since = "0.10.3")
  def lifted[G[_]]: LiftedPA[G] = new LiftedPA[G]

  @nowarn("cat=deprecation")
  private[transactor] final class LiftedPA[G[_]](private val dummy: Boolean = true) extends AnyVal {
    def apply[F[_]: BracketThrow](t: Transactor[F])(implicit L: Lift[F, G]): Txr.Lifted[G] =
      new Txr.Lifted[G] {
        val trans: ConnectionIO ~> G    = liftTrans(t.trans)
        val rawTrans: ConnectionIO ~> G = liftTrans(t.rawTrans)

        private def liftTrans(fk: ConnectionIO ~> F): ConnectionIO ~> G = fk andThen L.liftF

        val transP: Stream[ConnectionIO, *] ~> Stream[G, *]    = liftTransP(t.transP)
        val rawTransP: Stream[ConnectionIO, *] ~> Stream[G, *] = liftTransP(t.rawTransP)

        private def liftTransP(fk: Stream[ConnectionIO, *] ~> Stream[F, *]): Stream[ConnectionIO, *] ~> Stream[G, *] =
          funK(s => fk(s).translate(L.liftF))
      }
  }

  /** Creates a contextual facade that lifts the effect of `Transactor` from `F[_]` to `G[_]` given `G HasContext R`
    * with `ConnectionRIO[R, *]` as the database effect.
    */
  @deprecated("Use `Transactor.mapK` and `Txr.continuational` as a better alternative", since = "0.10.3")
  def contextual[G[_]]: ContextualPA[G] = new ContextualPA[G]

  @nowarn("cat=deprecation")
  private[transactor] final class ContextualPA[G[_]](private val dummy: Boolean = true) extends AnyVal {
    def apply[F[_]: BracketThrow, R](
        t: Transactor[F]
    )(implicit L: Lift[F, G], G: Monad[G], C: G WithContext R): Txr.Contextual[G, R] =
      new Txr.Contextual[G, R] {
        val trans: ConnectionRIO[R, *] ~> G    = liftTrans(t.trans)
        val rawTrans: ConnectionRIO[R, *] ~> G = liftTrans(t.rawTrans)

        private def liftTrans(fk: ConnectionIO ~> F): ConnectionRIO[R, *] ~> G =
          funK(crio => C.askF(ctx => L.lift(fk(crio.run(ctx)))))

        val transP: Stream[ConnectionRIO[R, *], *] ~> Stream[G, *]    = liftTransPK(t.transPK)
        val rawTransP: Stream[ConnectionRIO[R, *], *] ~> Stream[G, *] = liftTransPK(t.rawTransPK)

        private def liftTransPK(
            fk: Stream[ConnectionRIO[R, *], *] ~> Stream[ReaderT[F, R, *], *]
        ): Stream[ConnectionRIO[R, *], *] ~> Stream[G, *] =
          funK(s => fk(s).translate(funKFrom[ReaderT[F, R, *]](rio => C.askF(ctx => L.lift(rio.run(ctx))))))
      }
  }

}
