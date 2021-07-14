package tofu.doobie

import tofu.doobie.transactor.Txr
import tofu.syntax.doobie.txr._

object TxrSuite {

  def transSyntaxTest[F[_], DB[_]](dba: DB[Int])(implicit txr: Txr[F, DB]): Any = {
    dba.trans
    dba.rawTrans
  }

  def transStreamSyntaxTest[F[_], DB[_]](sdba: fs2.Stream[DB, Int])(implicit txr: Txr[F, DB]): Any = {
    sdba.trans
    sdba.rawTrans
  }

}
