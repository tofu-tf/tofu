---
id: tofu.logging.recipes
title: Tofu Logging recipes
---


# Recipes

Here is the list of ways how one can use `tofu.logging`

For any of the recipes you are going to need three things:
- ensure all the data types which you want to log have instances of [Loggable typeclass](../main-entities.md#typeclass-loggablea);
- import syntax: `import tofu.syntax.logging._`
- provide correct logback.xml (except when you are using `tofu.logging` with your own implementation) — you can find example [here](https://github.com/tofu-tf/tofu/tree/better-doobie-example/examples).

Recipes are:

- [Simple logging](simple.md) — when you just need to make the task done
- [Service logging](service.md) — same as latter but with a bit less code to write
- [Automatic logging](auto.md) derivation — when you don't even want to write log messages and your apps' modularization rocks
- [Custom logs](custom.md) — when you need to do something else when you log

And of course you can mix and match all of these to build what you want.


