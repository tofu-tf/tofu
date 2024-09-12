---
id: tofu.logging.loggable
title: Loggable typeclass
---

To represent a value in logs we use a concept of `Loggable` (it's a typeclass). It describes how a value of some type
can be logged, both as a string representation in log message and as a component of structured logging. Given an
instance of `Loggable` for a type, a value of the type can be converted into the final internal representation
called `LoggedValue` and thus logged in a way that you provided.  
There are multiple predefined ways to create an instance of `Loggable`, many of them can be found
in `tofu.logging.Loggable` object:

* `Loggable.empty` for no-op logging of value
* `Loggable.show` for using `cats.Show` instance as string representation
* `Loggable.either` for logging either of `A` and `B`
* provided instances for all primitive types, as well as stdlib's collections and collections from Cats
* java.time.* instances

Of course, you can describe your `Loggable` instance yourself by extending existing traits that pre-implement some
functionality:

* `DictLoggable` for multi-field objects
* `ToStringLoggable` for using `.toString` for logging
* `HideLoggable` to exclude value from logging

### Loggable example

```scala:reset
import tofu.logging._
import cats.syntax.semigroup._

case class User(name: String, surname: String)

implicit val userLoggable = new DictLoggable[User] {
  override def fields[I, V, R, S](a: User, i: I)(implicit r: LogRenderer[I, V, R, S]): R = {
    r.addString("name", a.name, i) |+| r.addString("surname", a.surname, i)
  }

  override def logShow(a: User): String = s"name = ${a.name}, surname = ${a.surname}"
}
```

Let's take a look at this example.  
First, we define a loggable for our `User` class as a `DictLoggable` which means that we want to log it as a multi-field
object with structure.  
Second, we define two methods that describe how `User` should be logged:

* `fields`, that represents `User` as a structure, containing two fields with their respective names and values
* `logShow`, that represents `User` as a string in a log message

## Loggable derivation

a
Tofu has integration with [derevo](https://github.com/tofu-tf/derevo) library. It allows you to easily generate
instances of `Loggable[YourClass]` for case classes or ADTs:

```scala:reset
import tofu.logging.derivation.loggable
import derevo.derive

@derive(loggable)
case class Query(value: String, size: Int, isTagged: Tagged, tag: Tag)

@derive(loggable)
case class Tagged(b: Option[Boolean])

@derive(loggable)
sealed trait Tag

@derive(loggable)
case class DatabaseTag(db: String) extends Tag

@derive(loggable)
case object InMemoryTag extends Tag
```

so when logged

### Configuring Loggable generation

Tofu has several annotations to configure generation of `Loggable`

* `@hidden`
  ```scala:reset
  import tofu.logging.derivation.{loggable, hidden}
  import derevo.derive
  
  @derive(loggable)
  case class User(name: String, @hidden() password: String)
  
  ```
  `User("tofu", "pass")` will be logged as `User{name=tofu}`
* `@masked`
    ```scala:reset
  import tofu.logging.derivation.{loggable, masked, MaskMode}
  import derevo.derive
  
  @derive(loggable)
  case class Card(@masked(MaskMode.Erase) expirationDate: String, @masked(MaskMode.ForLength(4, 12)) cardNumber: String, @masked() owner: String)
  
  ```
  `Card(737, "1244345632322311", "TOFU PETROV")` will be logged as
  `Card{expirationDate=...,cardNumber=1244########2311,owner=**** ******}`
* `@unembed`
   ```scala:reset
  import tofu.logging.derivation.{loggable, unembed}
  import derevo.derive
  
  @derive(loggable)
  case class Scientist(@unembed owner: Person, age: Option[Int])
  
  @derive(loggable)
  case class Person(name: String)
  ```
  `Room(Person("Karl Sagan"), None)` will be logged as `Room{name=Karl Sagan, age=None}`

These annotations belong to a separate dependency-free package `logging-derivation-annotations`.
Main `logging-derivation` package depends on it.
It allows for adding these annotations right to your domain classes if there's a need to keep it without dependencies.