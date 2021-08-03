---
id: tofu.logging.main.entities
title: tofu.logging
---


# tofu.logging core concepts

`tofu.logging` consists of three main things:

## Typeclass `Loggable[A]`
This is a type class that describes how an arbitrary instance of type A should be presented in the log messages.

## Derivation
Loggable instances could be derived automatically using `derevo`:

```scala
import tofu.logging.derivation._

@derive(loggable)
case class Data(id: Long, weight: Int, name: String)
```

This annotation puts the instance of `Loggable[Data]` into the (generated if not present) companion object `Data`.
When the message is logged, the fields of `Data` will be put into the result (e.g. JSON).

### Configured derivation

This derivation can be configured with annotations `hidden`, `masked` and `unembed`:
```scala
import tofu.logging.derivation._
@derive(loggable)
case class ClientData(name: String, surname: String)

@derive(loggable)
case class Payment(id: Long, @masked(MaskMode.Erase) cardNumber: String, @unembed name: ClientData)
```

A message `info"This is $payment"` would look like that:
```json
{
  "message": "This is Payment",
  "payment.id": 434324,
  "to": "do"
}
```


## Logging

`Logging[F]` is a trait that describes logging capabilities of `F`.

```scala
trait Logging[F[_]]{
  def info(message: String, values: LoggedValue*): F[Unit]
  //...
}
```

It has a tagged version `ServiceLogging[F, Service]` which can be used for some `Service` and carry this information on the typelevel.

## Logs

Trait `Logs[I, F]` is a factory of the `Logging[F]` instances.

As the creation of arbitrary logging instance could potentially have some side effects,
operations of this trait are effectual: 
```scala
trait Logs[I[_], F[_]]{
  def byName(name: String): I[Logging[F]]
  def forService[Svc]: I[Logging[F]]
  //...
}
```
The name method parameter (or type tag for `Svc` type parameter) is used in the underlying logger and then displayed in the log messages.


### Logging.Make
Nevertheless, some Logging instances can be created safely with no side effects, so one could use `Logging.Make`
which creates plain `Logging[F]`. It uses the default backend by `Slf4j` and `Delay` typeclass. (todo: Link to doc about delay).
