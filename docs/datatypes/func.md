# Func and AppFunc

API Documentation: @:api(cats.data.Func)

Func is a wrapper around a `run` function `A => F[B]` where `F` is a functor. Given that, the Func data type is equipped with the known `map` function, and a `mapK` function to apply natural transformations (from a `Func[F[_], A, B]` get an `Func[G[_], A, B]`).

The signature `Func[F[_], A, B]` is very similar to the signature for [Kleisli](../datatypes/kleisli.md): `Kleisli[F[_], -A, B]`. The difference is that `Func` is a less restrictive data type that wraps around functors, and only provides basic methods `run`, `map`, and `mapK`, while `Kleisli` is strong enough to provide composition, flatMap, and more. We will see a more useful data type just next with `AppFunc`. 

## Quick example

```scala mdoc:silent:nest
import cats.data.{ Func, AppFunc }
import cats._

val f: Func[List, Int, String] = Func.func((x: Int) => List(x.toString))

val g: Func[List, Int, Option[String]] = f.map((x: String) => if (x=="0") None else Some(x))

val optToList = new (Option ~> List) {
  def apply[T](opt: Option[T]): List[T] =
    opt.toList
}

// We transform the elements of List, of type 
// Option[String] to List[String]
g.map(optToList(_))
// val res0: cats.data.Func[List,Int,List[String]] = ...
```



# AppFunc 

AppFunc extends Func to wrap around a special type of functor: [Applicative] functors.

With applicative functors we can `compose`, form the `product`, and also `traverse` traversable functors

Signature: `AppFunc[F[_], A, B] extends Func[F, A, B]` 

Now, for the reader familiar with [Kleisli](../datatypes/kleisli.md), we find an even more similar data type. `AppFunc` provides compositions of weaker constraint, allowing `AppFunc[F[_], A, B]` to be composed with `AppFunc[G[_], C, A]`.   
## Composition

All of functional programming revolves around composing, and functors cannot be left behind. If we are working with multiple contexts we might want to compose them, for example: we want to `List` things, and discard some (`Option`). 

To achieve this nested context behavior `AppFunc` uses the [`Nested`] datatype. 

```scala mdoc:silent:nest
val appFuncOption: AppFunc[Option,Int,Int] = Func.appFunc((i: Int) => if (i==0) None else Some(i))

val appFuncList: AppFunc[List,Int,Int] = Func.appFunc((o: Int) => {List(o+1)})
(appFuncOption andThen appFuncList).run(1) //Nested(Some(List(2)))

(appFuncOption andThen appFuncList).run(0) //Nested(None)
// same thing with compose

(appFuncList compose appFuncOption)
```
## Product

Applicative functors, like monads, are closed under product. Cats models product of two applicative functors (they can be different!) in the @:api(cats.data.Tuple2K) data type. 

For further reading: [herding cats](http://eed3si9n.com/herding-cats/combining-applicative.html#Product+of+applicative+functions) 

```scala mdoc:silent:nest
(appFuncOption product appFuncList).run(1)
```
## Traverse

This explained in the implementation of the Applicative trait: [Applicative - Traverse](https://typelevel.org/cats/typeclasses/applicative.html#traverse)

```scala mdoc:silent:nest
appFuncOption.traverse(List(1,2,3))
// val res14: Option[List[Int]] = Some(List(1, 2, 3))

appFuncOption.traverse(List(1,2,0))
//val res15: Option[List[Int]] = None
```

