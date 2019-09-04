# ToFu

| CI | Release | 
| --- | --- |
| [![Build Status](https://travis-ci.com/TinkoffCreditSystems/tofu.svg?branch=master)](https://travis-ci.com/TinkoffCreditSystems/tofu) | [![Maven Central](https://img.shields.io/maven-central/v/ru.tinkoff/tofu-core_2.13.svg)](https://search.maven.org/search?q=ru.tinkoff.tofu-core) | 

**Functional programming toolkit by Tinkoff scala team aimed at taming the complexity of Tagless Final approach.** 

### Quick Start

To use the whole utils pack just add to your `build.sbt`: 

```scala
libraryDependencies += "ru.tinkoff" %% "tofu" % "0.3.0"
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


_Here will be glorious readme soon with link to the gorgeous microsite_

