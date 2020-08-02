# Kind numbers

Naming kinds is hard. Suppose you have `Function1[A, B] = A => B` and `FunctionK[F[_], G[_]] = [A] => F[A] => G[A]`

Suppose you have `T[_[_, _], _, _]` how to correctly name transformation between such types? `FunctionK_KK_KK` ?  
Alphabetical codes could be hard to read and disambiguate. So I propose enumerate them all with a simple scheme.

Here we suppose that `* = Type` is a Kind of  inhabitable types and `k1 -> k2` where `k1` and `k2` are kind is itself kind that has single type argument of kind `k1` and result of kind `k2`. Those arrows in absence of parenthesis are associated right.
All scala kinds we suppose to be naturally curried.  

So `T[_[_, _], _, _]` becomes `(* -> * -> *) -> * -> * -> *` or  `(* -> (* -> *)) -> (* -> (* -> *))`
and `T[_[_[_]], _[_[_]]]` becomes `((* -> *) -> *) -> ((* -> *) -> *) -> *` or `((* -> *) -> *) -> (((* -> *) -> *) -> *)`.

We can consider those elements as binary trees, elements of  

```scala mdoc
sealed trait Bin
case object Leaf extends Bin
case class Branch(l: Bin, r: Bin) extends Bin
```

`*` corresponds to `Leaf` and `k1 -> k2` corresponds to `Branch(k1, k2)`  
For those trees we defined rank as following:

```scala mdoc
lazy val rank: Bin => Int = {
    case Leaf => 0
    case Branch(b1, b2) => (rank(b1) max rank(b2)) + 1
}
```  

We can calculate count of different binary trees for each rank as follows:

```scala mdoc
val sizeUpToRank: LazyList[BigInt] = LazyList.from(0).map{x =>  
    if(x == 0) 0 else rankSize(x - 1) + sizeUpToRank(x - 1)
}

val rankSize: LazyList[BigInt] = LazyList.from(0).map{x =>
    if(x <= 1) 1  
    else 2 * sizeUpToRank(x - 1) * rankSize(x - 1) + rankSize(x - 1) * rankSize(x - 1)
}
```

Now we can assign unique natural index to Binary trees inside the rank and globally

```scala mdoc
lazy val rankIndex: Bin => BigInt = {
    case Leaf => 0
    case tree@Branch(tl, tr) =>
        val r   = rank(tree) - 1
        val q   = rankSize(r)
        val u   = sizeUpToRank(r) * q
        if(rank(tl) < r)
            index(tl) * q + rankIndex(tr)
        else if(rank(tr) < r)
            index(tr) * q + rankIndex(tl) + u  
        else  
            rankIndex(tl) * q + rankIndex(tr) + 2 * u
}

def index(tree: Bin): BigInt = sizeUpToRank(rank(tree)) + rankIndex(tree)

index(Branch(Branch(Leaf, Leaf), Leaf))
```

We can provide string representations for our trees.

```scala mdoc
lazy val args: Bin => LazyList[Bin] = {
    case Leaf => LazyList.empty
    case Branch(t1, t2) => t1 #:: args(t2)
}

lazy val toStarString: Bin => String = {
    case Leaf => "*"
    case tree => args(tree).iterator.map {
        case Leaf => "*"
        case branch => s"(${toStarString(branch)})"
    }.mkString("", " -> ", " -> *")
}

lazy val toScalaString: Bin => String = {
    case Leaf => "T"
    case tree => args(tree).iterator.map {
        case Leaf => "_"
        case branch => s"_${toScalaString(branch)}"
    }.mkString("[", ", ", "]")
}
```

Now we can generate some trees

```scala mdoc
val trees = LazyList.iterate((LazyList[Bin](), LazyList[Bin](Leaf))){ case (prev, cur) =>  
    val xs = for(tl <- prev; tr <- cur) yield Branch(tl, tr)
    val ys = for(tr <- prev; tl <- cur) yield Branch(tl, tr)
    val zs = for(tl <- cur; tr <- cur)  yield Branch(tl, tr)
    val next = xs #::: ys #::: zs
    (prev #::: cur, next)
}.flatMap(_._2).map(t => (toScalaString(t), index(t)))

```

Here we can find that our desired shape for `[_[_, _], _, _]` has number 17

```scala mdoc
trees.find(_._1 == "[_[_, _], _, _]").get._2
```

Therefore we can name our kind-specific transformation as `FunctionKn17` meaning this is transformation between typeconstructors of type with index 17 having following form

```scala
type FunctionKn17[A[_[_, _], _, _], B[_[_, _], _, _]] = [F[_, _], E, A] => A[F, E, A] => B[F, E, A]
```

We can also reverse kind getting by index

```scala mdoc
def byIndex(x: BigInt): Bin = if(x == 0) Leaf else {
    val r = sizeUpToRank.indexWhere(_ > x) - 1
    byIndexInRank(x - sizeUpToRank(r), r)
}

def byIndexInRank(x: BigInt, r: Int): Bin = if(r == 0) Leaf else {
    val q = rankSize(r - 1)
    val u = sizeUpToRank(r - 1) * q
    if(x < u)
        Branch(byIndex(x / q), byIndexInRank(x % q, r - 1))
    else if(x < 2 * u){
        val x1 = x - u
        Branch(byIndexInRank(x1 % q, r - 1), byIndex(x1 / q))
    } else {
        val x1 = x - 2 * u
        Branch(byIndexInRank(x1 / q, r - 1), byIndexInRank(x1 % q, r - 1))
    }
}
```

we may check that these functions are indeed isomorphim in natural numbers

```scala mdoc
Iterator.range(0, 100).forall(i => index(byIndex(i)) == i)
```

So now if one need to get signature for Kind number 17, they may write

```scala mdoc
toScalaString(byIndex(17))
toStarString(byIndex(17))
```
