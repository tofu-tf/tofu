package tofu.doobie
package transactor

import cats.data.Kleisli
import cats.effect.{MonadCancelThrow, Resource}
import cats.~>
import doobie.{ConnectionIO, Transactor}
import fs2.Stream
import tofu.syntax.funk._
import tofu.syntax.monadic._

/** A simple facade for [[doobie.Transactor]] that holds an inner database effect type `DB[_]` and provides natural
  * transformations from this effect to the target effect `F[_]`.
  *
  * The motivation for using this facade instead of `Transactor` is to:
  *
  *   - initialize all its natural transformations early and remove the need for additional constraints on `F[_]` later
  *     (e.g. `Bracket`);
  *
  *   - be able to use another `DB[_]` effect besides `ConnectionIO` and build a layer of transactional logic with it.
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
  def plain[F[_]: MonadCancelThrow](t: Transactor[F]): Txr.Plain[F] =
    new Txr.Plain[F] {
      val trans: ConnectionIO ~> F    = t.trans
      val rawTrans: ConnectionIO ~> F = t.rawTrans

      val transP: Stream[ConnectionIO, *] ~> Stream[F, *]    = t.transP
      val rawTransP: Stream[ConnectionIO, *] ~> Stream[F, *] = t.rawTransP
    }

  /** Creates a facade that uses `ConnectionCIO` as the database effect.
    */
  def continuational[F[_]: MonadCancelThrow](t: Transactor[F]): Txr.Continuational[F] =
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
}
