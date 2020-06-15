# Kind numbers with variance support

Naming kinds is hard. Especially with variance
Consider `Function1[-A, +B] = A => B` and `FunctionK[-F[_], +G[_]] = [A] => F[A] => G[A]`

Suppose you have now `T[_[-_, +_], -_, +_]` how to correctly name transformation between such types? `FunctionK_PM_PM` ?  
Alphabetical codes could be hard to read and disambiguate. So I propose enumerate them all with a simple scheme.

Here we suppose that `* = Type` is a Kind of inhabitable types and `[v]k1 -> k2` where `k1` and `k2` are kinds is itself a kind that has single type argument of kind `k1` and result of kind `k2`. And `[v]` is a variance annotation that could be `+`, `-` or empty, meaning invariant argument
 Those arrows in absence of parenthesis are associated right.

All scala kinds we suppose to be naturally curried.  

So `T[_[-_, +_], -_, +_]`  
becomes  `(-* -> +* -> *) -> -* -> -* -> *`  
or  `(-* -> (+* -> *)) -> (-* -> (+* -> *))`  
and `T[-_[_[_]], +_[_[_]]]`  
becomes `-((* -> *) -> *) -> +((* -> *) -> *) -> *`  
or `-((* -> *) -> *) -> +(((* -> *) -> *) -> *)`.

We can consider those elements as binary trees, elements of  

```scala mdoc
sealed trait Variance
case object VP extends Variance // plus  : covariant
case object VM extends Variance // minux : contravariant
case object VZ extends Variance // zero  : invariant

sealed trait Bin
case object Leaf extends Bin
case class Branch(v: Variance, l: Bin, r: Bin) extends Bin
```

`*` corresponds to `Leaf` and `[v]k1 -> k2` corresponds to `Branch(v, k1, k2)`  
For such trees we define rank as following:

```scala mdoc
lazy val rank: Bin => Int = {
    case Leaf => 0
    case Branch(_, b1, b2) => (rank(b1) max rank(b2)) + 1
}
```  

We can calculate count of different binary trees for each rank as follows:

```scala mdoc
val sizeUpToRank: LazyList[BigInt] = LazyList.from(0).map{x =>  
    if(x == 0) 0 else rankSize(x - 1) + sizeUpToRank(x - 1)
}

val rankSize: LazyList[BigInt] = LazyList.from(0).map{x =>
    if(x == 0) 1  
    else 3 * (2 * sizeUpToRank(x - 1) * rankSize(x - 1) + rankSize(x - 1) * rankSize(x - 1))
}
```

Now we can assign unique natural index to Binary trees inside the rank and globally

```scala mdoc
lazy val varianceIndex: Variance => Int = {
    case VP => 0
    case VM => 1
    case VZ => 2
}

lazy val rankIndex: Bin => BigInt = {
    case Leaf => 0
    case tree@Branch(v, tl, tr) =>
        val r   = rank(tree) - 1
        val q   = rankSize(r)
        val u   = sizeUpToRank(r) * q
        val idx =  
        if(rank(tl) < r)
            index(tl) * q + rankIndex(tr)
        else if(rank(tr) < r)
            index(tr) * q + rankIndex(tl) + u  
        else  
            rankIndex(tl) * q + rankIndex(tr) + 2 * u
        idx * 3 + varianceIndex(v)
}

def index(tree: Bin): BigInt = sizeUpToRank(rank(tree)) + rankIndex(tree)

index(Branch(VM , Branch(VP, Leaf, Leaf), Leaf))
```

We can provide string representations for our trees.

```scala mdoc
lazy val varianceSign: Variance => String = {
    case VP => "+"
    case VM => "-"
    case VZ => ""
}

lazy val args: Bin => LazyList[(String, Bin)] = {
    case Leaf => LazyList.empty
    case Branch(v, t1, t2) => (varianceSign(v), t1) #:: args(t2)
}

lazy val toStarString: Bin => String = {
    case Leaf => "*"
    case tree => args(tree).iterator.map {
        case (sign, Leaf) => s"$sign*"
        case (sign, branch) => s"$sign(${toStarString(branch)})"
    }.mkString("", " -> ", " -> *")
}

lazy val toScalaString: Bin => String = {
    case Leaf => "T"
    case tree => args(tree).iterator.map {
        case (sign, Leaf) => s"${sign}_"
        case (sign, branch) => s"${sign}_${toScalaString(branch)}"
    }.mkString("[", ", ", "]")
}
```

Now we can generate some trees

```scala mdoc
val variances = LazyList(VP, VM, VZ)
val trees = LazyList.iterate((LazyList[Bin](), LazyList[Bin](Leaf))){ case (prev, cur) =>  
    val xs = for(tl <- prev; tr <- cur; v <- variances) yield Branch(v, tl, tr)
    val ys = for(tr <- prev; tl <- cur; v <- variances) yield Branch(v, tl, tr)
    val zs = for(tl <- cur; tr <- cur; v <- variances)  yield Branch(v, tl, tr)
    val next = xs #::: ys #::: zs
    (prev #::: cur, next)
}.flatMap(_._2).map(t => (index(t), toScalaString(t), toStarString(t)))
```

Here we can find that our desired shape for `[_[-_, +_], -_, +_]` has number 1269

```scala mdoc
trees.find(_._2 == "[_[-_, +_], -_, +_]").get._1
```

Therefore we can name our kind-specific transformation as `FunctionVn1269` meaning this is transformation between typeconstructors of type with index 1269 having following form

```scala
type FunctionVn1269[A[_[-_, +_], -_, +_], B[_[-_, +_], -_, +_]] = [F[-_, +_], E, A] => A[F, E, A] => B[F, E, A]
```

We can also reverse kind getting by index

```scala mdoc
def byIndex(x: BigInt): Bin = if(x == 0) Leaf else {
    val r = sizeUpToRank.indexWhere(_ > x) - 1
    byIndexInRank(x - sizeUpToRank(r), r)
}

def varByIndex: Int => Variance = {
    case 0 => VP
    case 1 => VM
    case 2 => VZ
}

def byIndexInRank(x: BigInt, r: Int): Bin = if(r == 0) Leaf else {
    val q = rankSize(r - 1)
    val u = sizeUpToRank(r - 1) * q
    val v = varByIndex((x % 3).toInt)
    val x0 = x / 3
    if(x0 < u)
        Branch(v, byIndex(x0 / q), byIndexInRank(x0 % q, r - 1))
    else if(x0 < 2 * u){
        val x1 = x0 - u
        Branch(v, byIndexInRank(x1 % q, r - 1), byIndex(x1 / q))
    } else {
        val x1 = x0 - 2 * u
        Branch(v, byIndexInRank(x1 / q, r - 1), byIndexInRank(x1 % q, r - 1))
    }
}
```

we may check that these functions are indeed isomorphim in natural numbers

```scala mdoc
Iterator.range(0, 1000).forall(i => index(byIndex(i)) == i)
```

So now if one need to get signature for Kind number 17, they may write

```scala mdoc
toScalaString(byIndex(1269))
toStarString(byIndex(1269))
```
