---
title: Recipes list
sidebar:
  order: 101
---

Here is the list of ways how one can use `tofu.logging`

For any of the recipes you are going to need three things:

- ensure all the data types which you want to log have instances
  of [Loggable typeclass](/docs/logging/core-concepts/#typeclass-loggablea);
- import syntax: `import tofu.syntax.logging._`
- provide correct logback configuration (except when you are using `tofu.logging` with your own implementation) — you
  can find the description [here](/docs/logging/layouts) and the
  example [here](https://github.com/tofu-tf/tofu/blob/master/examples/ce2/src/main/resources/logback.groovy).

Recipes are:

- [Simple logging](/docs/logging/recipes/simple) — when you just need to make the task done
- [Service logging](/docs/logging/recipes/service) — same as latter but with a bit less code to write
- [Automatic logging](/docs/logging/recipes/auto) derivation — when you don't even want to write log messages and your apps' modularization
  rocks
- [Custom logs](/docs/logging/recipes/custom) — when you need to do something else when you log

And of course you can mix and match all of these to build what you want.

