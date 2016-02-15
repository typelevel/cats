package cats.fix

import cats.{Applicative, Functor}
import cats.syntax.functor._

/**
  *
  * Note: this is NOT library code
  *
  * it is an example illustrating library code
  *
  * should eventually be removed
  *
  */


object FixApp {


  def main(args: Array[String]): Unit = {

    trait ListFunctor[Z, A]
    case class Nil[Z, A]() extends ListFunctor[Z, A]
    case class Cons[Z, A](z: Z, a: A) extends ListFunctor[Z, A]

    def listFunctor[Z] = new Functor[ListFunctor[Z, ?]] {
      def map[A, B](lfa: ListFunctor[Z, A])(a2b: A => B): ListFunctor[Z, B] = lfa match {
        case Nil() => Nil()
        case Cons(z, a) => Cons(z, a2b(a))
      }
    }

    type List[Z] = Fix[ListFunctor[Z, ?]]

    def nil[Z](implicit implicitListFunctor: Functor[ListFunctor[Z, ?]]): List[Z] =
      Fix[ListFunctor[Z, ?]](Nil())
    def cons[Z](implicit implicitListFunctor: Functor[ListFunctor[Z, ?]]): (Z, List[Z]) => List[Z] = {
      (z, zs) => Fix[ListFunctor[Z, ?]](Cons(z, zs))
    }

    def showListAlgebra[Z]: ListFunctor[Z, String] => String = {
      case Nil() => "nil"
      case Cons(z, string) => s"cons($z, $string)"
    }

    def showList[Z]: List[Z] => String =
      _.fold[String](showListAlgebra)

    object ListTraversableFunctor extends FixTraverse[ListFunctor] {
      implicit def implicitFunctor[Z] = listFunctor[Z]

      /**
        *
        * for `Nil` (having 0 parameters) we use `map0` (agreed, actually `pure`)
        * for `Cons` (having 2 parameters) we use `map2`
        *
        * there should be a way to generalize this to *all polynomial functors*
        *
        */
      def traverseAlgebra[A[_] : Applicative, Z, Y](z2ay: Z => A[Y]): ListFunctor[Z, A[Fix[ListFunctor[Y, ?]]]] => A[Fix[ListFunctor[Y, ?]]] = {
        case Nil() => Applicative[A].pure(nil[Y])
        case Cons(z, a_ys) => Applicative[A].map2(z2ay(z), a_ys)(cons[Y])
      }
      def foldLeft[Z, Y](fix: Fix[ListFunctor[Z, ?]], y: Y)(yz2y: (Y, Z) => Y): Y = fix.fold[Y] {
        case Nil() => y
        case Cons(z, y) => yz2y(y, z)
      }
      def foldRight[Z, Y](fix: Fix[ListFunctor[Z, ?]], ly: cats.Eval[Y])(zly2ly: (Z, cats.Eval[Y]) => cats.Eval[Y]): cats.Eval[Y] = fix.fold[cats.Eval[Y]] {
        case Nil() => ly
        case Cons(z, ly) => zly2ly(z, ly)
      }
    }


    import ListTraversableFunctor.traverse

    
    /**
      * Traversing lists of strings
      * while trying to parse their elements as integers
      *
      * lists
      *
      */

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

    import cats.std.either.eitherInstances

    implicit val implicitStringListFunctor = listFunctor[String]

    val goodOneTwoThreeStrings =
      cons.apply("1", cons.apply("2", cons.apply("3", nil[String])))

    val badOneTwoThreeStrings =
      cons.apply("1", cons.apply("two", cons.apply("3", nil[String])))

    val goodOptionalOneTwoThreeInts: Try[List[Int]] =
      traverse[Try, String, Int](goodOneTwoThreeStrings) { s => `try`(Integer.parseInt(s)) }

    val badOptionalOneTwoThreeInts: Try[List[Int]] =
      traverse[Try, String, Int](badOneTwoThreeStrings) { s => `try`(Integer.parseInt(s)) }

    println(goodOptionalOneTwoThreeInts.map(showList[Int]))

    println(badOptionalOneTwoThreeInts.map(showList[Int]))

  }

}
