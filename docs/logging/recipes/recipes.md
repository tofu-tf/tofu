---
id: tofu.logging.recipes
title: Tofu Logging recipes
---


# Recipes

Here is the list of ways how one can use `tofu.logging`

For any of the recipes you going to need two things:
- Ensure all the data types you log have instance of Loggable (todo link to core concepts);
- import syntax: `import tofu.syntax.logging._`

Recipes are:

- [Simple logging](simple.md) — when you just need to make the task done
- [Service logging](service.md) — same as latter but with a bit less code to write
- [Automatic logging](auto.md) derivation — when you don't even want to write log messages and your apps' modularization rocks
- [Custom logs](custom.md) — when you need to do something else when you log

And of course you can mix and match all of these to build what you want.


