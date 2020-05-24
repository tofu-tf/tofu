package tofu.examples

import cats.FlatMap
import cats.syntax.flatMap._
import tofu.Raise
import tofu.syntax.raise._

final case class User(id: Long, cardId: Long, name: String)
final case class Card(id: Long, balance: Long)

sealed trait WithdrawalFailed extends Throwable

final case class CardNotFound(id: Long) extends WithdrawalFailed
final case class UserNotFound(id: Long) extends WithdrawalFailed
final case class LowBalance(actual: Long, required: Long) extends WithdrawalFailed

trait CardStorage[F[_]] {
  def get(cardId: Long): F[Card]
  def updateBalance(cardId: Long, newBalance: Long): F[Unit]
}

object CardStorage {

  def make[F[_]: Raise[*[_], CardNotFound]]: CardStorage[F] = new Impl[F]

  private final class Impl[F[_]: Raise[*[_], CardNotFound]] extends CardStorage[F] {
    override def get(cardId: Long): F[Card] = ???
    override def updateBalance(cardId: Long, newBalance: Long): F[Unit] = ???
  }
}

trait UserStorage[F[_]] {
  def get(userId: Long): F[User]
}

object UserStorage {

  def make[F[_]: Raise[*[_], UserNotFound]]: UserStorage[F] = new Impl[F]

  private final class Impl[F[_]: Raise[*[_], UserNotFound]] extends UserStorage[F] {
    override def get(userId: Long): F[User] = ???
  }
}

trait WithdrawalService[F[_]] {
  def withdraw(userId: Long, amount: Long): F[Unit]
}

object WithdrawalService {

  def make[F[_]: Raise[*[_], WithdrawalFailed]: FlatMap]: WithdrawalService[F] =
    new Impl[F](UserStorage.make, CardStorage.make)

  final class Impl[F[_]: Raise[*[_], WithdrawalFailed]: FlatMap](
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
