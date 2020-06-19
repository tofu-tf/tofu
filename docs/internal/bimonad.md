# Bimonad tutorial

Bifunctors have become a hot topic in the Scala community since ZIO introduced the second type parameter.
It's a great way to manage business errors in your code, but it lacks suitable abstractions.
The most comprehensive set I've seen lies [here](https://github.com/7mind/izumi/tree/develop/fundamentals/fundamentals-bio/src/main/scala/izumi/functional/bio).
But it's built upon practical usage.
So let's start building a more principled approach. 

## Monads

Let's talk about the basic definitions. We will use the basic category theory terminology, most of which can be found on [Wikipedia](https://en.wikipedia.org/wiki/Monad_(functional_programming)) and [nLab](https://ncatlab.org/nlab/show/monad).
We will use the following definitions.

### Category 
Starting from this section, we will think of a Category as some entity with some additional structure.
The class mentioned below is something like a Set or a Type. Its elements are objects of the category. Since we want the object class to be arbitrary big (bigger than the classic Set could be) and we do not need equality/identity relation on it, we will denote it _Class_.


A [category](https://en.wikipedia.org/wiki/Category_(mathematics)) `C` is a Class called _objects_ together with an indexed set family `Hom: (C, C) -> Set` and functions

```
id : ∀(a: C), Hom(a, a)

∘ : ∀{a, b, c : C}, (Hom(b, c), Hom(a, b)) -> Hom(a, c) 
```
and the following properties
```
left-id : ∀{a, b: C}, ∀(f: Hom(a, b)), id b ∘ f = f

right-id : ∀{a, b: C}, ∀(f: Hom(a, b)), f ∘ id a = f

associativity : ∀{a, b, c, d: C}, ∀(f: Hom(c, d)), ∀(g: Hom(b, c)), ∀(h: Hom(c, d)), ∀(h: Hom(a, b)), f ∘ (g ∘ h) = (f ∘ g) ∘ h
```

The most acknowledged category in a functional programming language is a `Set`-like category of inhabitable types and pure total functions between them, so the objects are types, and for types `A` and `B` `Hom(A, B)` is a set of pure functions `A => B`. For Scala, we will call it `Scala`.

Another sort of category that Scala types form is subtyping relations, i.e. we have a single element `Hom(A, B)` when `A <: B` and an empty Hom otherwise.

### Functor

A functor `F` between categories `C` and `D` is a function `C -> D` together with a family of functions

```
map: ∀(a, b: C), Hom(a, b) -> Hom(F(a), F(b))
```
and the following properties
```
functor-id: ∀(a: C), map(F)(id a) = id b
functor-compose: ∀(a, b, c: C), ∀(f: Hom(b, c), g: Hom(a, b)),  map{F}(f ∘ g) = map{F}(f) ∘ map{F}(g)
```

The most beloved form of functors are endo-functors `Scala` -> `Scala`, whose interface looks like
```scala
trait Functor[F[_]]{
  def map[A, B](f: A => B): F[A] => F[B]
}
```
or more commonly in the uncurried form 
```scala
trait Functor[F[_]]{
  def map[A, B](fa: F[A])(f: A => B): F[B]
}
```
properties are assumed for implementations

there are some other interesting functors, for example
```scala
trait Invariant[F[_]]{
    def imap[A, B](fa: F[A])(f: A => B)(g: B => A): F[B]
}
```
is an interface for functors from a Scala-[core](https://ncatlab.org/nlab/show/core), i.e. category of all Scala isomorphisms to Scala

Notable endofunctor for any category is identity functor `Id` such that 
```
Id(a) = a
map{Id}(f) = f
```

Also for any triple of categories C, D, E, having functors `F: D -> E`  and `G: C -> D` we can define composition of functors
```text
(F ∘ G)(a) = F(G(a))
map{F ∘ G}(f) = map{F}(map{G}(f))
```
[^1]

### Transformations

This section has a lot of unnecessary info, so you can skip it 

Having categories `C` and `D`,  functors `F, G : C -> D`,  natural transformation `ф : F -> G` 
```
∀(a: C), F(a) -> G(a)
```
together with property

```
naturality: ∀(a, b: C), f: Hom(a, b), ф{b} ∘ map{F}(f) = map{G}(f) ∘ ф{a}
```

in scala, if we are talking about endofunctors `Scala -> Scala` we consider it having the following interface

```scala
trait FunctionK[F[_], G[_]]{ 
  def apply[A](fa: F[A]): G[A]
}
```
Still `F` and `G` don't have to be Functors at all, so we can talk about just transformation (not natural) between type-constructors.[^2]

For natural transformations, we can talk about identity and composition as well.
For any functor F, identity transformation is defined by
```
tid{F}{a} = id{F(a)}
```

Moreover for categories `C`, `D` and functors `F`, `G`, `H` from C to D,  (vertical) composition of transformations `ф : G -> H` and `ц : F -> G` is defined as
```text
(ф ∘ ц){a} = ф{a} ∘ ц{a}
```

We can apply functor to the transformation at whole, supposing element-wise application, i.e. having categories
`C` ,`D`, `E` and functors `F: C -> D`, `G: C -> D` and `H : D -> E`, transformation `ф : F -> G`, we define new transformation from `H ∘ F` to `H ∘ G` as application `H` to `ф` : 

```text
H(ф){a} = map{H}(ф{a})
```

Also we can apply transformation to a functor, i.e. having categories
`C` ,`D`, `E` and functors `F: D -> E`, `G: D -> E` and `H : C -> D`, transformation `ф : F -> G`, we define new transformation from `F ∘ H` to `G ∘ H` as an application of `ф` to `H` :
```text
ф(H){a} = ф{H(a)}
```

Generalizing that we can define the horizontal composition of transformations.
For any categories`C, D, E` , functors `F, G: D -> E` and `H, I : C -> D` and transformations `ф: F -> G` `ц: H -> I` , horizontal composition
`ф ● ц : F ∘ H -> G ∘ I` is defined as 

```text
ф ● ц {a} = ф{G(a)} ∘ map{H}(ц{a})
```
We can say that `ф ● ц = ф(G) ∘ H(ц)`, or `ф(F) =  ф ● tid{F}` and `F(ф) = tid{F} ● ф`
### Monad

The traditional definition of a monad is the following: Monad `F` in category `C` is an endofunctor together with following natural transformations:

```text
ν : Id -> F
μ : F ∘ F -> F
```

and properties

```text
left-identity:  μ ∘ ν(F) = tid{F}
right-identity: μ ∘ F(ν) = tid{F}
associativity:  μ ∘ μ(F) = μ ∘ F(μ)
```
These definitions should lead us to the definition of Monad in `Scala` like that

```scala
trait Monad[F[_]] extends Functor[F]{
    def map[A, B](f: A => B): F[A] => F[B]
    def pure[A](a: A): F[A]
    def flatten[A](ffa: F[F[A]]): F[A]
}
```
It has three methods to define and seven properties to prove (functor-id, functor-compose, naturality of pure, naturality of `flatten`, `left-identity`, `right-identity` and `associativity`)

Meanwhile, even in general form in category theory, we can merge all the definitions into one and get a Hom-form of Monad definition

So, monad `F` in a category C is a function `C -> C` (e.g. unary type-constructor in Scala), together with the following function families
```
pure: ∀a: C, Hom(a, F(a))
flatMap: ∀(a, b: C), Hom(a, F(b)) -> Hom(F(a), F(b))
```
together with the following laws
```
left-identity: ∀(a, b: C), f: Hom(a, b), flatMap(f) ∘ pure{b} = f
right-identity: ∀a: C, flatMap(pure{a}) = id{F(a)}
associativity: ∀(a, b, c: C),f: Hom(b, F(c)), g: Hom(a, F(b)), flatMap(f) ∘ flatMap(g) = flatMap(flatMap(f) ∘ g)
```
using these properties we can prove all seven properties for the original monad and vice versa. So it's just another definition for the same abstraction

that immediately gives us another form for monad interface
```scala
trait Monad[F[_]]{
    def pure[A](a: A): F[A]
    def flatMap[A, B](f: A => F[B]): F[A] => F[B]
}
```
or in the uncurried version
```scala
trait Monad[F[_]]{
    def pure[A](a: A): F[A]
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
}
```
Here we need to prove three well-known monad laws. Then functor laws, naturality restrictions, and original monad laws arise immediately.

## Bifunctors

### Product of categories

For any categories `C` and `D` we can define their product `C × D` as a category, which objects are pairs `(a, x)` where `a` is an object of `C` and `x` is an object of `D` 
Then `Hom((a, x), (b, y))` is a `Hom(a, b) × Hom(x, y)`, i.e. set of pairs `(f, u)` where `f` is an element of `Hom(a, b)` and `u` is an element of `Hom(x, y)`

Rest of the structure defined as follows
```text
id{(a, x)} = (id{a}, id{x})
(f, u) ∘ (g, v) = (f ∘ g, u ∘ v)
```
And properties are easy to prove.

### Bifunctor

Bifunctor is just a word meaning "functor from the product of categories", so bifunctor `C × D -> E` is just a functor `C × D -> E` 

Technically that means that we have a binary function on objects and 
mapping function, that mapping couple of functions into a single one.

So bifunctor `Scala × Scala -> Scala` would have following interface 
```scala
trait Bifunctor[F[_, _]]{
    def bimap[A, B, C, D](f: A => B, g: C => D): F[A, B] => F[C, D]
}
```

One notable property of bifunctors is orthogonality of mappings, meaning

```
(f, id) ∘ (id, g) = (f ∘ id, id ∘ g) = (f, g) = (id ∘ f, g ∘ id) = 
    (id, g) ∘ (f, id)
```
and for bifunctor `F` it would mean that 

```text
map{F}(f, id) ∘ map{F}(id, g) = map{F}(id, g) ∘ map{F}(f, id)
```
Which looks like some form of commutativity, but is not since `id`s have different implicit indices

But it allows to define bifunctor in the following form
```scala 
trait Bifunctor[F[_, _]]{
    def lmap[A, B, C](f: A => B): F[A, C] => F[B, C]
    def rmap[A, B, C](f: A => B): F[C, A] => F[C, A]
}
```
So we definitely may split any mappings to left and right components

### Bifunctor monad

So what is a monad which is also bifunctor? It should be an endofunctor from a product of some categories, implying that it's a functor which results also in a product of these categories,

```
pure: ∀a: C, x: D, Hom((a, x), F(a, x))
flatMap: ∀(a, b: C), (x, y: D), Hom((a, x), F(b, y)) -> Hom(F(a, x), F(b, y))
```

But here we must remember that F is a function to a pair of objects.  
How can we represent such thing on scala types?  
We can immediately think of any function resulting in a pair as a pair of functions, producing corresponding elements of pair.
Meaning that `F: A -> B × C` can be represented `F_1: A -> B` and `F_2: A -> C` and `F(a) = (F_1(a), F_2(a))`.  
So a binary function from types to a pair of types we can represent as a pair of type constructors.
Same trick will work for components of `pure` and `flatMap` : instead of tuple of functions as a result we define two different methods for each component
There for a bifunctor monad 
`Scala × Scala -> Scala × Scala` would look as 
```scala
trait BiMonad[L[_, _], R[_, _]]{
    def pureLeft[A, B](a: A): L[A, B]
    def pureRight[A, B](b: B): R[A, B]
    def flatMapLeft[A, B, C, D](fl: A => L[C, D], fr: B => R[C, D]): L[A, B] => L[C, D]
    def flatMapRight[A, B, C, D](fl: A => L[C, D], fr: B => R[C, D]): R[A, B] => R[C, D]
}
```
We can easily uncurry that and define bifunctors for both `L` and `R` using such definitions.

### Twin monad

Such definitions are still unsettling. It is hard to imagine how it could be useful having two different type-constructors. So the most obvious way of narrowing our definition is requiring that 
`R = L`, then our definitions of `flatMapLeft` and `flatMapRight` become identical and can be further merged into one method.

So now we with just three methods for which we can imagine fancy names.

`pureLeft` we will call `raise` as it was raising errors
`pureRight` we will call `pure` since right side we decide to be the _right side_
and merged `flatMap` we will call `foldWith` since it provides a way to continue from both success and error and has some resemblance with `fold` method on well-known bifunctors like `Either` now we get our glorious and uncurried

```scala
trait TwinMonad[F[_, _]]{
    def raise[A, B](a: A): F[A, B]
    def success[A, B](b: B): F[A, B]
    def foldWith[A, B, C, D](fab: F[A, B])(f: A => F[C, D], g: B => F[C, D]): F[C, D]
}
```

Following the Monad laws, we derive 4 fundamental laws for the TwinMonad[^3]

In the following code all the parameters (types and values) implied to be universally quantifined (_for all_) , and the resulting equality type `:=:` - is the required property
```scala
    def raiseFold[E1, A1, E2, A2](e: E1)(f: E1 => F[E2, A2])(g: A1 => F[E2, A2]) : 
        foldWith(raise(e))(f, g) :=: f(e)
    
    def successFold[E1, A1, E2, A2](a: A1)(f: E1 => F[E2, A2])(g: A1 => F[E2, A2]) : 
        foldWith(raise(e))(f, g) :=: g(a)

    def foldPure[E, A](fea: F[E, A]) : foldWith(fea)(raise)(success) :=: fea

    def foldAssoc[E1, A1, E2, A2, E3, A3](
        fea: F[E, A]
    )(
        f1: E1 => F[E2, A2], g1: A1 => F[E2, A2]
    )(
        f2: E2 => F[E3, A3], g2: A2 => F[E3, A3]
    ): foldWith(foldWith(fea)(f1, g1))(f1, g1) :=: foldWith(fea)(e => foldWith(f1(e))(f2, g2), a => foldWith(g1(a))(f2, g2qq)
```

### Symmetry

so right from this, we may define the following method

```scala
def swap[E, A](fea: F[E, A]): F[A, E] = foldWith(fea)(success, raise)
```

and directly from the laws we can prove following lemmas
```scala
def swapInvolutive[E, A](fea: F[E, A]): swap(swap(fea)) :=: fea
def swapRaise[E, A](e: E): swap(raise(e)) :=: success(e)
def swapSuccess[E, A](a: A): swap(success(a)) :=: raise(e)
def swap-fold[E1, A1, E2, A2](fea: F[E1, A1])(f: E1 => F[E2, A2])(g: A1 => F[E2, A2]) : 
    foldWith(swap(fea)(g, f) = foldWith(fea)(f, g)
```

So our monad has a powerful quality of symmetry. We require that monad treats errors and successes equally, so we can switch between them whenever we want.
All types that can't satisfy this property we reject.
Moreover, in all the abstractions on top of `TwinMonad`, we'll search for the same symmetry, trying not to think of one binding channel more than of another.
That will help us to find correct shapes in the binary monad universe.

### Stack safety

To restore the same stack-safe qualities that are already in the `cats` we add another iterative form of `foldWith` called `foldRec`.
`def foldRec[E, A, X, B](init: Either[E, A])(step: Either[E, A] => F[Either[E, X], Either[A, B]]): F[X, B]` - here `E` and `A` are type parameters for the intermediate results, while we have `Left(e: E)` on the error channel or `Left(a: A)` on the right channel the iteration continues. As soon as we reached `Right(x :X)` on the left or `Right(b: B)` on the right, we must stop, finalizing the result. 

Resulting bifunctor brother of `cats.Monad` we call `Bind` which I feel is nice for two reasons:
1. It starts with `Bi` so it's easy to recall it when you are thinking about bifunctors
2. Its name resembles is a common alias for the `flatMap` method, so it's easy to recall it as a `Monad`

[^1]: We may consider a category of categories, where functors are morphisms. Such functors would play the role of an identity morphism and composition here
[^2]: Note that if we consider the category of scala types, where all `Hom`s are empty, any type constructor there would be a functor, and any transformation would be natural.
[^3]: These and others proofs can be found in [our Arend repository dedicated to Category Theory](https://github.com/Odomontois/Tincat/blob/master/src/Functor/Monad.ard)
