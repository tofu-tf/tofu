package tofu.syntax.doobie

import fs2.Stream
import tofu.doobie.transactor.Txr

private[doobie] trait TxrSyntax {
  implicit def toTxrOps[DB[_], A](dba: DB[A]): TxrOps[DB, A]                     = new TxrOps(dba)
  implicit def toTxrStreamOps[DB[_], A](dba: Stream[DB, A]): TxrStreamOps[DB, A] = new TxrStreamOps(dba)
}

private[doobie] final class TxrOps[DB[_], A](private val dba: DB[A]) extends AnyVal {
  def trans[F[_]](implicit txr: Txr[F, DB]): F[A]    = txr.trans(dba)
  def rawTrans[F[_]](implicit txr: Txr[F, DB]): F[A] = txr.rawTrans(dba)
}

private[doobie] final class TxrStreamOps[DB[_], A](private val sdba: Stream[DB, A]) {
  def trans[F[_]](implicit txr: Txr[F, DB]): Stream[F, A]    = txr.transP(sdba)
  def rawTrans[F[_]](implicit txr: Txr[F, DB]): Stream[F, A] = txr.rawTransP(sdba)
}
