---
layout: default
title:  "Fixed Points of Polynomial Functors"
section: "data"
source: "https://github.com/non/cats/blob/master/core/src/main/scala/cats/data/Poly.scala"
scaladoc: "#cats.data.Poly"
---------------------------
# Fixed Points of Polynomial Functors

*Polynomial functors* are functors (see [`Functor`](functor.html)) that are, *recursively*, defined in terms of 

 * `Constant`, cfr. *constants* `a`, `b`, `c`, ... , but also carrying *data*
 * `Identity`, cfr. a *variable* `z` , but also carrying *data*
 * `Product`, cfr. *product* `*` , but also carrying *data*
 * `Sum`, cfr. *sum* `+` , but also carrying *data*
 
 Many recursive data structures can be seen as least fixed points (see [`Fix`](fix.html)) of polynomial functors
 
  * `List` corresponds to the *linear polynomial* `u + (a * z)` (`u` being a special *unit constant* not carrying any data)
  * `Tree` corresponds to the *quadraticpolynomial*  `a + (z * z)`
  
  
# An example: lists
  
```tut
import cats.Functor
import cats.data.Types._
import cats.data._
import cats.fix.Types._
import cats.fix.Fix

```

Let's start with some convenient *type aliases* (whose names you'll probably recognize)
  
```tut
  type Nil[Y, Z] = Constant[Unit, Z]
  type Head[Y, Z] = Constant[Y, Z]
  type Tail[Y, Z] = Identity[Y, Z]
  type Cons[Y, Z] = Prod[λ[ζ => Head[Y, ζ]], λ[ζ => Tail[Y, ζ]], Z]
  type List[Y, Z] = Xor[Nil[Y, Z], Cons[Y, Z]]

```
 
Next we can define corresponding *functors*
 
```tut
  def nilFunctor[Y]: Functor[λ[ζ => Nil[Y, ζ]]] = new ConstantFunctor[Unit]
  def headFunctor[Y]: Functor[λ[ζ => Head[Y, ζ]]] = new ConstantFunctor[Y]
  def tailFunctor[Y]: Functor[λ[ζ => Tail[Y, ζ]]] = new IdentityFunctor[Y]
  def consFunctor[Y]: Functor[λ[ζ => Cons[Y, ζ]]] = new ProductFunctor[Head, Tail, Y](headFunctor[Y], tailFunctor[Y])
  
```

and an implicit functor

```tut
  implicit def listFunctor[Y]: Functor[λ[ζ => List[Y, ζ]]] = new SumFunctor[Nil, Cons, Y](nilFunctor[Y], consFunctor[Y])
  
```

now we are ready to define corresponding *constructors*

```tut
  def cons[Y, Z](y: Y, z: Z): Cons[Y, Z] = Prod[λ[ζ => Head[Y, ζ]], λ[ζ => Tail[Y, ζ]], Z](y, z)

  def nilList[Y, Z]: Unit => List[Y, Z] = _ => Xor.Left[Nil[Y, Z]](())
  def consList[Y, Z]: (Y, Z) => List[Y, Z] = (y, z) => Xor.Right[Cons[Y, Z]](cons(y, z))

```

and, once we have defined the *least fixed point*

```tut
  type FixList[Z] = Fix[λ[ζ => List[Z, ζ]]]

```

and corresponding *function*
```tut
  def fixList[Z]: List[Z, FixList[Z]] => FixList[Z] = Fix[λ[ζ => List[Z, ζ]]]
  
```

we can define corresponding constructors

```tut
  def nil[Y](implicit implicitListFunctor: Functor[λ[ζ => List[Y, ζ]]]): Unit => FixList[Y] =
    _ => fixList[Y](nilList[Y, FixList[Y]](()))
  def cons[Y](implicit implicitListFunctor: Functor[λ[ζ => List[Y, ζ]]]): (Y, FixList[Y]) => FixList[Y] =
    (y, ys) => fixList[Y](consList[Y, FixList[Y]](y, ys))
    
```

Finally we are ready for some *algebra*'s
```tut
  type ListAlgebra[Y, Z] = Algebra[λ[ζ => List[Y, ζ]], Z]

  def listAlgebra[Y, A]: (A, (Y, A) => A) => List[Y, A] => A = (a, ya2a) =>
    _.fold(nil => a, cons => ya2a(cons.first, cons.second))

  def showListAlgebra[Y]: ListAlgebra[Y, String] =
    listAlgebra("Nil", _ + " :: " + _)
 
  def incrIntListAlgebra: ListAlgebra[Int, FixList[Int]] =
    listAlgebra(nil[Int].apply(()), (i, is) => cons[Int].apply(i + 1, is))  
     
```

and some corresponding *structurally recursive functions on lists*
```tut
  def showFixList[Y]: FixList[Y] => String =
    _.cata[String](showListAlgebra)

  def incrIntFixList: FixList[Int] => FixList[Int] =
    _.cata[ FixList[Int]](incrIntListAlgebra)

```

and we can use all we defined as follows:
evaluating `showFixList[Int](incrIntFixList(cons[Int].apply(1, cons[Int].apply(2, nil[Int].apply(())))))` 
results in `2 :: 3 :: Nil`


# So what's the big deal?

Well. the big deal is that, up to working with `a + (z * z)` instead of `u + (a * z)`,
the code for `Tree` is almost exactly the same as the code for `List`


  
# A very similar example: trees
  

Let's start with some convenient *type aliases* (whose names you'll probably recognize)
  
```tut
  type Leaf[Y, Z] = Constant[Y, Z]
  type Branch[Y, Z] = Identity[Y, Z]
  type Fork[Y, Z] = Prod[λ[ζ => Branch[Y, ζ]], λ[ζ => Branch[Y, ζ]], Z]
  type Tree[Y, Z] = Xor[Leaf[Y, Z], Fork[Y, Z]]

```
 
Next we can define corresponding *functors*
 
```tut
  def leafFunctor[Y]: Functor[λ[ζ => Leaf[Y, ζ]]] = new ConstantFunctor[Y]
  def branchFunctor[Y]: Functor[λ[ζ => Branch[Y, ζ]]] = new IdentityFunctor[Y]
  def forkFunctor[Y]: Functor[λ[ζ => Fork[Y, ζ]]] = new ProductFunctor[Branch, Branch, Y](branchFunctor[Y], branchFunctor[Y])
  
```

and an implicit functor

```tut
  implicit def treeFunctor[Y]: Functor[λ[ζ => Tree[Y, ζ]]] = new SumFunctor[Leaf, Fork, Y](leafFunctor[Y], forkFunctor[Y])
  
```

now we are ready to define corresponding *constructors*

```tut

  def fork[Y, Z](lz: Z, rz: Z): Fork[Y, Z] = Prod[λ[ζ => Branch[Y, ζ]], λ[ζ => Branch[Y, ζ]], Z](lz, rz)

  def leafTree[Y, Z]: Y => Tree[Y, Z] = y => Xor.Left[Leaf[Y, Z]](y)
  def forkTree[Y, Z]: (Z, Z) => Tree[Y, Z] = (lz, rz) => Xor.Right[Fork[Y, Z]](fork(lz, rz))
  
```

and, once we have defined the *least fixed point*

```tut
  type FixTree[Z] = Fix[λ[ζ => Tree[Z, ζ]]]
  
```

and corresponding *function*
```tut
  def fixTree[Z]: Tree[Z, FixTree[Z]] => FixTree[Z] = Fix[λ[ζ => Tree[Z, ζ]]]

```

we can define corresponding constructors

```tut
  def leaf[Y](implicit implicitTreeFunctor: Functor[λ[ζ => Tree[Y, ζ]]]): Y => FixTree[Y] =
    y => fixTree[Y](leafTree[Y, FixTree[Y]](y))
  def fork[Y](implicit implicitTreeFunctor: Functor[λ[ζ => Tree[Y, ζ]]]): (FixTree[Y], FixTree[Y]) => FixTree[Y] =
    (lt, rt) => fixTree[Y](forkTree[Y, FixTree[Y]](lt, rt))

```

Finally we are ready for some *algebra*'s
```tut
  type TreeAlgebra[Y, Z] = Algebra[λ[ζ => Tree[Y, ζ]], Z]

  def treeAlgebra[Y, A]: (Y => A, ((A, A) => A)) => Tree[Y, A] => A = (y2a, aa2a) =>
    _.fold(leaf => y2a(leaf), fork => aa2a(fork.first, fork.second))

  def showTreeAlgebra[Y]: TreeAlgebra[Y, String] =
    treeAlgebra("Leaf(" + _ + ")", "(" + _ + " ^ " + _ + ")")

  def incrIntTreeAlgebra: TreeAlgebra[Int, FixTree[Int]] =
    treeAlgebra(i => leaf[Int].apply(i + 1), (ft, st) => fork[Int].apply(ft, st))  

```

and some corresponding *structurally recursive functions on trees*
```tut
  def showFixTree[Y]: FixTree[Y] => String =
    _.cata[String](showTreeAlgebra)

  def incrIntFixTree: FixTree[Int] => FixTree[Int] =
    _.cata[ FixTree[Int]](incrIntTreeAlgebra)

```

and we can use all we defined as follows:
evaluating `showFixTree[Int](incrIntFixTree(fork[Int].apply(fork[Int].apply(leaf[Int].apply(1), leaf[Int].apply(2)), fork[Int].apply(leaf[Int].apply(3), leaf[Int].apply(4)))))` 
results in `((Leaf(2) ^ Leaf(3)) ^ (Leaf(4) ^ Leaf(5)))`




 
 

