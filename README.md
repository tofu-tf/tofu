<img align="right" src="logos/tofu-mascot.png" height="170px" style="padding-left: 20px"/>

# ToFu

**Functional programming toolkit aimed at taming the complexity of Tagless Final approach.** 

| CI | Release | Chat |
| --- | --- | --- |
| ![Scala CI](https://github.com/TinkoffCreditSystems/tofu/workflows/Scala%20CI/badge.svg) | [![Maven Central](https://img.shields.io/maven-central/v/ru.tinkoff/tofu-core_2.13.svg)](https://search.maven.org/search?q=ru.tinkoff.tofu-core) | [![Discord Chat](https://img.shields.io/discord/657318688025739283.svg)](https://discord.gg/qPD5GGH)

# Quick Start

## see [the docs on the microsite](https://tinkoffcreditsystems.github.io/tofu)

To use the whole utils pack just add to your `build.sbt`: 

```scala
libraryDependencies += "ru.tinkoff" %% "tofu" % "latest version in badge"
```

Of course, you can also specify an exact list of `tofu` modules that you want to add to your dependencies (used in place
of `"tofu"`):

* `tofu-core` for core utils
* `tofu-core-higher-kind` for higher kinded utils
* `tofu-concurrent` for concurrent utils
* `tofu-config` for config utils
* `tofu-data` for data utils
* `tofu-derivation` for derivation utils and [derevo](https://github.com/manatki/derevo) annotations
* `tofu-doobie` for [Doobie](https://github.com/tpolecat/doobie) utils
* `tofu-enums` for [Enumeratum](https://github.com/lloydmeta/enumeratum) utils
* `tofu-env` for `Env` (a variation of a Reader Monad based on [Monix](https://github.com/monix/monix) Task)
* `tofu-fs2-interop` for interop with [fs2](https://github.com/functional-streams-for-scala/fs2)
* `tofu-streams` for streaming utils
* `tofu-logging` for the whole set of logging utils (derivation, layout, structured, util, interop)
  * `tofu-logging-derivation` for logging derivation only
  * `tofu-logging-layout` for logging layout only
  * `tofu-logging-structured` for logging structured only
  * `tofu-logging-util` for logging util only
  * `tofu-logging-refined` for interop between tofu-logging and [Refined](https://github.com/fthomas/refined) 
  * `tofu-logging-shapeless` for interop between tofu-logging and [Shapeless](https://github.com/milessabin/shapeless) tag
* `tofu-memo` for caching utils
* `tofu-observable` for `monix.reactive.Observable` utils
* `tofu-optics-core` for optics core (Optics typeclasses)
* `tofu-optics-interop` for optics interop with [Monocle](https://github.com/julien-truffaut/Monocle)
* `tofu-optics-macro` for macro optics generators
* `tofu-zio-interop` for interop with [ZIO](https://zio.dev) (core, logging)
  * `tofu-zio-core` for ZIO instances only
  * `tofu-zio-logging` for ZIO logging only

<img align="right" src="logos/tofu-logo.png" height="100px" style="padding-left: 5px"/>


# Adopters

Proud user of ToFu? Feel free to [add your company!](https://github.com/TinkoffCreditSystems/tofu/edit/master/README.md)

<a href="https://tinkoff.ru/"><img width="40%" src="logos/yandex-travel-logo.svg?sanitize=true" /></a>

<a href="https://tele2.ru/"><img width="40%" src="logos/tele2-ru-logo.svg?sanitize=true" /></a>

<a href="https://konfy.care/"><img width="40%" src="logos/konfy-logo.svg?sanitize=true" /></a>

<a href="https://www.raiffeisen.ru/en/"><img width="40%" src="logos/raiffeisen-logo.svg?sanitize=true" alt="Raiffeisen Bank Russia"/></a>

# Contributing

Please note we use the following labels for automated release descriptions:
  * `chore` if your PR does not change any types and runtime semantics
  * `fix` if your PR merely fixes incorrect behavior

## Formatting
  We have an automated check for style conformance. You can run `sbt checkfmt` before PR.
  If you have any trouble during this check, just run `sbt fmt` and commit again.
  
# Copyright
Copyright the maintainers, 2020

Logos made with love by [@impurepics](https://twitter.com/impurepics)
