<img align="right" src="logos/tofu-mascot.png" height="170px" style="padding-left: 20px"/>

# ToFu

**Functional programming toolkit by Tinkoff scala team aimed at taming the complexity of Tagless Final approach.** 

| CI | Release | Chat |
| --- | --- | --- |
| ![Scala CI](https://github.com/TinkoffCreditSystems/tofu/workflows/Scala%20CI/badge.svg) | [![Maven Central](https://img.shields.io/maven-central/v/ru.tinkoff/tofu-core_2.13.svg)](https://search.maven.org/search?q=ru.tinkoff.tofu-core) | [![Discord Chat](https://img.shields.io/discord/657318688025739283.svg)](https://discord.gg/qPD5GGH)

# Quick Start

## see [the docs on the microsite](https://tinkoffcreditsystems.github.io/tofu)

To use the whole utils pack just add to your `build.sbt`: 

```scala
libraryDependencies += "ru.tinkoff" %% "tofu" % "latest version in badge"
```

Of course you can also specify the exact subproject that you wanna add to yours dependencies (used in place of `"tofu"`):

* `tofu-core` for core (Main core)
* `tofu-memo` for memo (Caching utils)
* `tofu-env` for env (Some variation of a Reader Monad)
* `tofu-parallel` for parallel (Parallel utils)
* `tofu-concurrent` for concurrent (Concurrent utils)
* `tofu-optics-core` for optics core (Optics typeclasses)
* `tofu-optics-interop` for optics interop with [Monocle](https://github.com/julien-truffaut/Monocle)
* `tofu-optics-macro` for macro optics generators
* `tofu-data` for data utils
* `tofu-logging` for the whole set of logging utils (derivation, layout, structured)
* `tofu-logging-derivation` for logging derivation only
* `tofu-logging-structured` for logging structured only
* `tofu-logging-layout` for logging layout only
* `tofu-observable` for observable
* `tofu-enums` for enums ([Enumeratum](https://github.com/lloydmeta/enumeratum) utils)

<img align="right" src="logos/tofu-logo.png" height="100px" style="padding-left: 5px"/>

# Copyright
Copyright the maintainers, 2019

Logos made with love by [@impurepics](https://twitter.com/impurepics)

# Contributing

Please note we use following labels for automated release descriptions:
  * `chore` if your PR does not change any types and runtime semantics
  * `fix` if your PR merely fixes incorrect behavior

## Formatting
  We have automated check for style conformance. You can run `sbt fmt` before PR.
  If you have any trouble during this check, just run `sbt fmt` and commit again.
