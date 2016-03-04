---
layout: default
title:  "Fixed Points"
section: "fix"
source: "https://github.com/non/cats/blob/master/core/src/main/scala/cats/fix/Fix.scala"
scaladoc: "#cats.fix.Fix"
-------------------------
# Fixed Points

`Fix` uses the [`Functor`](functor.html) type class to define a *structural recursive* function `cata` over the fixed point `Fix[F]` of a functor `F`.
As such, `cata` can be seen as a generalization of the function `foldRight` on `List[A]`.

In order to `traverse` the least fixed point of a functor `F`, resulting in a value of type `A[Fix[F[Y, ?]]]`,
it suffices to define an appropriate algebra of type `F[Z, A[Fix[F[Y, ?]]]] => A[Fix[F[Y, ?]]]`.

The nice thing about algebras of type `F[A] => A` as opposed to the structural recursive functions of type `Fix[F] => A` defined in terms of them
is that they can be defined in a *modular* way because *recursion* and *algebraic functor operations* (*sum* and *product*) are strictly separated.
See [Using Catamorphisms Subtypes and Monad Transformers for Writing Modular Functional Interpreters](https://www.researchgate.net/publication/2550340_Using_Catamorphisms_Subtypes_and_Monad_Transformers_for_Writing_Modular_Functional_Interpreters) for more information.

## Fixed Point example: imports

```tut
import cats.{Applicative, Functor, Show, Eval, Now, Later, Always}
import cats.syntax.functor._
import cats.fix._
import cats.std.either.eitherInstances
```


## Fixed Point example: List

Let's look at an example: *lists* as least fixed points of a functor:

```tut
    trait ListFunctor[Z, A]
    case class Nil[Z, A]() extends ListFunctor[Z, A]
    case class Cons[Z, A](z: Z, a: A) extends ListFunctor[Z, A]
    
    def listFunctor[Z] = new Functor[ListFunctor[Z, ?]] {
      def map[A, B](lfa: ListFunctor[Z, A])(a2b: A => B): ListFunctor[Z, B] = lfa match {
        case Nil() => Nil()
        case Cons(z, a) => Cons(z, a2b(a))
      }
    }   
```

```tut
    type List[Z] = Fix[ListFunctor[Z, ?]]
```

Like it is always the case with case classes it is convenient to define some *list constructors*:

```tut
    def nil[Z](implicit implicitListFunctor: Functor[ListFunctor[Z, ?]]): List[Z] =
      Fix[ListFunctor[Z, ?]](Nil())
    def cons[Z](implicit implicitListFunctor: Functor[ListFunctor[Z, ?]]): (Z, List[Z]) => List[Z] = {
      (z, zs) => Fix[ListFunctor[Z, ?]](Cons(z, zs))
    }
```

## List Fixed Point example: Show

So how do we *show* lists.
It suffices to define an `implicit def implicitShowList[Z]: Show[List[Z]]`.
For this it suffices to define a function `def show(list: List[Z]): String`.
For this it suffices to define an algebra `def showListAlgebra[Z]: ListFunctor[Z, String] => String`.

So here we go:

```tut
    def showListAlgebra[Z]: ListFunctor[Z, String] => String = {
      case Nil() => "nil"
      case Cons(z, string) => s"cons($z, $string)"
    }

    def showList[Z]: List[Z] => String =
      _.cata[String](showListAlgebra)

    implicit def implicitShowList[Z]: Show[List[Z]] = new Show[List[Z]] {
      def show(list: List[Z]): String = showList(list)
    }
```  

## List Fixed Point example: Traverse

So how do we *traverse* lists.
It suffices to define an `object ListTraversableFunctor extends FixTraverse[ListFunctor]`.
For this it suffices to define a algebra `def traverseAlgebra[A[_] : Applicative, Z, Y](z2ay: Z => A[Y]): ListFunctor[Z, A[Fix[ListFunctor[Y, ?]]]] => A[Fix[ListFunctor[Y, ?]]]`.

So here we go:
  
```tut
    object ListTraversableFunctor extends FixTraverse[ListFunctor] {
      implicit def implicitFunctor[Z] = listFunctor[Z]

      def traverseAlgebra[A[_] : Applicative, Z, Y](z2ay: Z => A[Y]): ListFunctor[Z, A[Fix[ListFunctor[Y, ?]]]] => A[Fix[ListFunctor[Y, ?]]] = {
        case Nil() => Applicative[A].pure(nil[Y])
        case Cons(z, a_ys) => Applicative[A].map2(z2ay(z), a_ys)(cons[Y])
      }

      def foldLeft[Z, Y](fix: Fix[ListFunctor[Z, ?]], y: Y)(yz2y: (Y, Z) => Y): Y = fix.cata[Y] {
        case Nil() => y
        case Cons(z, y) => yz2y(y, z)
      }

      def foldRight[Z, Y](fix: Fix[ListFunctor[Z, ?]], ly: Eval[Y])(zly2ly: (Z, Eval[Y]) => Eval[Y]): Eval[Y] = fix.cata[Eval[Y]] {
        case Nil() => ly
        case Cons(z, ly) => zly2ly(z, ly)
      }
    }
```

Note the usage of `Eval` (more about this later).

## Example: Traversing a list of strings, trying to parse them as integers

Here is a typical example: *traversing a list of strings, trying to parse them as integers*.

```tut
    import ListTraversableFunctor.traverse

    type Try[A] = Either[Throwable, A]

    def `try`[A](a: => A): Try[A] =
      try {
        Right(a)
      } catch {
        case throwable: Throwable => Left(throwable)
      }

    def parseToInt(string: String): Try[Int] =
      `try` {
        Integer.parseInt(string)
      }

    implicit val implicitStringListFunctor = listFunctor[String]

    val goodOneTwoThreeStrings =
      cons.apply("1", cons.apply("2", cons.apply("3", nil[String])))

    val badOneTwoThreeStrings =
      cons.apply("1", cons.apply("two", cons.apply("3", nil[String])))

    val goodTryingOneTwoThreeInts: Try[List[Int]] =
      traverse[Try, String, Int](goodOneTwoThreeStrings) { s => `try`(Integer.parseInt(s)) }

    val badTryingOneTwoThreeInts: Try[List[Int]] =
      traverse[Try, String, Int](badOneTwoThreeStrings) { s => `try`(Integer.parseInt(s)) }

```

## What about evaluation?

As you may know, structural recursion may benefit from different evaluation strategies.
Since `cata` works for *all* algebras, it suffices to replace `A` with one of the `Eval[]` case classes (`Now[A]`, `Later[A]` or `Always[A]`)

For example, `show` related stuff might as well have been defined as follows:

```tut
    def showListNowAlgebra[Z]: ListFunctor[Z, Now[String]] => Now[String] = {
      case Nil() => Now("nil")
      case Cons(z, evalString) => Now(s"cons($z, ${evalString.value})")
    }

    def showListLaterAlgebra[Z]: ListFunctor[Z, Later[String]] => Later[String] = {
      case Nil() => Later("nil")
      case Cons(z, evalString) => Later(s"cons($z, ${evalString.value})")
    }

    def showListAlwaysAlgebra[Z]: ListFunctor[Z, Always[String]] => Always[String] = {
      case Nil() => Always("nil")
      case Cons(z, evalString) => Always(s"cons($z, ${evalString.value})")
    }



    def showListNow[Z]: List[Z] => String =
      _.cata[Now[String]](showListNowAlgebra).value

    def showListLater[Z]: List[Z] => String =
      _.cata[Later[String]](showListLaterAlgebra).value

    def showListAlways[Z]: List[Z] => String =
      _.cata[Always[String]](showListAlwaysAlgebra).value

```




  
  


    




