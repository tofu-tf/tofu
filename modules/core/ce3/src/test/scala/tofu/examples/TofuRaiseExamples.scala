package tofu.examples

import cats.FlatMap
import cats.syntax.flatMap._
import tofu.compat.unused212
import tofu.syntax.handle._
import tofu.syntax.raise._
import tofu.{Errors, Handle, Raise}

final case class User(id: Long, cardId: Long, name: String)
final case class Card(id: Long, balance: Long)

sealed trait WithdrawalFailed extends Throwable

final case class CardNotFound(id: Long)                   extends WithdrawalFailed
final case class UserNotFound(id: Long)                   extends WithdrawalFailed
final case class LowBalance(actual: Long, required: Long) extends WithdrawalFailed

trait CardStorage[F[_]] {
  def get(cardId: Long): F[Card]
  def updateBalance(cardId: Long, newBalance: Long): F[Unit]
}

object CardStorage {

  def make[F[_]: ({ type L[x[_]] = Raise[x, CardNotFound] })#L]: CardStorage[F] = new Impl[F]

  @unused212
  private final class Impl[F[_]: ({ type L[x[_]] = Raise[x, CardNotFound] })#L] extends CardStorage[F] {
    override def get(cardId: Long): F[Card]                             = ???
    override def updateBalance(cardId: Long, newBalance: Long): F[Unit] = ???
  }
}

trait UserStorage[F[_]] {
  def get(userId: Long): F[User]
}

object UserStorage {

  def make[F[_]: ({ type L[x[_]] = Raise[x, UserNotFound] })#L]: UserStorage[F] = new Impl[F]

  @unused212
  private final class Impl[F[_]: ({ type L[x[_]] = Raise[x, UserNotFound] })#L] extends UserStorage[F] {
    override def get(userId: Long): F[User] = ???
  }
}

trait WithdrawalService[F[_]] {
  def withdraw(userId: Long, amount: Long): F[Unit]
}

object WithdrawalService {

  def make[F[_]: ({ type L[x[_]] = Raise[x, WithdrawalFailed] })#L: FlatMap]: WithdrawalService[F] =
    new Impl[F](UserStorage.make, CardStorage.make)

  private final class Impl[F[_]: ({ type L[x[_]] = Raise[x, WithdrawalFailed] })#L: FlatMap](
      userStorage: UserStorage[F],
      cardStorage: CardStorage[F]
  ) extends WithdrawalService[F] {
    override def withdraw(userId: Long, amount: Long): F[Unit] =
      userStorage.get(userId) >>= (u => cardStorage.get(u.cardId)) >>= { card =>
        val newBalance = card.balance - amount
        if (newBalance >= 0) cardStorage.updateBalance(card.id, newBalance)
        else LowBalance(card.balance, amount).raise[F, Unit]
      }
  }
}

trait Payments[F[_]] {
  def processPayment(userId: Long, amount: Long): F[Unit]
}

object Payments {

  def make[F[_]: ({ type L[x[_]] = Errors[x, WithdrawalFailed] })#L: FlatMap]: Payments[F] =
    new Impl[F](WithdrawalService.make)

  private final class Impl[F[_]: ({ type L[x[_]] = Handle[x, WithdrawalFailed] })#L](
      withdrawalService: WithdrawalService[F]
  ) extends Payments[F] {
    override def processPayment(userId: Long, amount: Long): F[Unit] =
      withdrawalService.withdraw(userId, amount).handleWith[LowBalance](_ => denyPayment)
    private def denyPayment: F[Unit]                                 = ???
  }
}
