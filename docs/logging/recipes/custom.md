---
id: tofu.logging.recipes.custom
title: Tofu Logging recipes
---


## TODO yet to be implemented

## Your own specific logs

tofu.logging allows you to use your own backend for logs, e.g. to send them to Clickhouse. To do this you'll need to
implement `Logs` trait. The rest is still handled by tofu.

Let's say you have this clickhouse client:

```scala
trait ClickhouseClient[F[_]] {
  def send[A](data: A): F[Unit]

  def initTable(name: String): F[Unit]
}
```

You'll need to implement only one method to create Logs:

```scala
def clickhouseLogs[F[_]](client: ClickhouseClient[F])(logs: Logs[F, F]) =
  new Logs[F, F] {
    def byName(name: String): F[Logging[F]] =
      for {
        _ <- client.initTable("logs")
        logging = logs.byName(name)
      } yield logging //todo how tf to implement this nicely?
  }
```