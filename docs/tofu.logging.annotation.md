---
id: tofu.logging.annotation
title: LogAnnotation
---


## LogAnnotation
Some data processing platforms have troubles with a large amount of log fields.   
One possible way to limit the number of fields your application logs is listening annotations and using them every time you log a value.

Another problem can occur if the same field is logged with different types (e.g. as a string and as a number). `LogAnnotation` statically types logging values. 
This helps also to store and retrieves values from data structures like `Map[LogAnnotation[_], Any]` (useful for zio-users who stores log context on `FiberRefs`).

### Exapmle

```scala
case class MyId(id: String)

object LogKey {
  val count: LogAnnotation[Int] = LogAnnotation.make[Int]("count")
  val myId: LogAnnotation[MyId] = Loggable[String].contramap((_: MyId).id).logAnnotation("id")
}

val maybeId = Some(MyId("123"))

log.info("Hello", LogKey.count -> 5, LogKey.myId -> maybeId)
// log.error("this won't compile", LogKey.count -> "5")

```
will produce:
```json
{"message":"Hello", "count":5, "id":"123"}
```
