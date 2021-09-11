---
id: tofu.logging.syntax
title: tofu.syntax.logging
---

# tofu.logging.syntax

All the usual methods are implemented with string interpolator:

There are basic methods:

- `info"This is INFO message`
- `warn"This is WARN message`
- `debug"This is DEBUG message`
- and so on.

And there are methods with errors:

- `errorCause"Oh no, an error!"(error)`
- `warnCause"Well, an error!"(error)`
- `infoCause"It's fine, just an error!"(error)`
- and so on.

One can add more information to the log structure of the message with these methods:

- `infoWith"I got data!"("key" -> "skeleton", "value" -> "too much", "price" -> 0)`
- `errorWith"I got data!"("key" -> "skeleton", "value" -> "too much", "price" -> 0)`
- and so on.
