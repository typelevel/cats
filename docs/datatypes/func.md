# Func

Func is a wrapper arround a `run` function `a => F[B]` where F is a functor. Given that, the Func data type is equiped with the known `map` function, and a `mapK` function to apply natural transformations (from a `F` Func get an `G` Func).

## Quick example

```scala mdoc:silent:nest
import cats.data.{ Func, AppFunc }
import cats._

val f: Func[List, Int, String] = Func.func((x: Int) => List(x.toString))

val g: Func[List, Int, Option[String]] = f.map((x: String) => if (x=="0") None else Some(x))

val natTransformation = new (Option ~> List) {
  def apply[T](opt: Option[T]): List[T] =
    opt.toList
}

// We transform the elements of List, of type 
// Option[String] to List[String]
g.map(natTransformation(_))
```



# AppFunc

AppFunc extends Func to wrap around a special type of functor: Applicative functors.

Applicative functors can be `compose`d, can `traverse` traversable functors, and we can obtain a `product` between two AppFuncs. 

## Composition

All of functional programming revolves around composing, and functors cannot be left behind. If we are working with multiple contexts we might want to compose them, for example: we have a List of executions, and want to List things, and discart some (`Option`). 

To achieve this nested context behavior `AppFunc` uses the `Nested` datatype. 

```scala mdoc:silent:nest
val AppFuncF: AppFunc[Option,Int,Int] = Func.appFunc((i: Int) => if (i==0) None else Some(i))

val AppFuncG: AppFunc[List,Int,Int] = Func.appFunc((o: Int) => {List(o+1)})
(AppFuncF andThen AppFuncG).run(1) //Nested(Some(List(2)))

(AppFuncF andThen AppFuncG).run(0) //Nested(None)

// same thing with compose

(AppFuncG compose AppFuncF)

```
## Product

Applicative functors, like monads, are closed under product. Cats models this data type as Tuple2k, allowing to form the product of two applicative functors (they can be different!) in one data type. 

For further reading: [hearding cats](http://eed3si9n.com/herding-cats/combining-applicative.html#Product+of+applicative+functions) 

```scala mdoc:silent:nest
(AppFuncF product AppFuncG).run(1)
```
## Traverse

Finally, the main subject of the [The Essence of the Iterator Pattern](https://www.cs.ox.ac.uk/jeremy.gibbons/publications/iterator.pdf) is that given the properties of the applictive functor, we can iterate over traversable types applying an applicative functor. 

```scala mdoc:silent:nest
AppFuncF.traverse(List(1,2,3))
// val res14: Option[List[Int]] = Some(List(1, 2, 3))

AppFuncF.traverse(List(1,2,0))
//val res15: Option[List[Int]] = None
```

