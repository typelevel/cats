package cats.instances
import cats.arrow.{ArrowChoice, CommutativeArrow}

trait PartialFunctionInstances {

  implicit def catsStdInstancesForPartialFunction: ArrowChoice[PartialFunction] with CommutativeArrow[PartialFunction] =
    PartialFunctionInstances.instance
}

private object PartialFunctionInstances {

  private val instance: ArrowChoice[PartialFunction] with CommutativeArrow[PartialFunction] =
    new ArrowChoice[PartialFunction] with CommutativeArrow[PartialFunction] {

      /**
       * {{{
       * scala> import cats.arrow.Arrow
       * scala> import cats.syntax.arrowChoice._
       * scala> val toLong: PartialFunction[Int, Long] = Arrow[PartialFunction].lift(_.toLong)
       * scala> val toDouble: PartialFunction[Float, Double] = Arrow[PartialFunction].lift(_.toDouble)
       * scala> val f: PartialFunction[Either[Int, Float], Either[Long, Double]] = toLong +++ toDouble
       * scala> f(Left(3))
       * res0: Either[Long,Double] = Left(3)
       * scala> f(Right(3))
       * res1: Either[Long,Double] = Right(3.0)
       * }}}
       */
      override def choose[A, B, C, D](
        f: PartialFunction[A, C]
      )(g: PartialFunction[B, D]): PartialFunction[Either[A, B], Either[C, D]] = {
        case Left(a) if f.isDefinedAt(a)  => Left(f(a))
        case Right(b) if g.isDefinedAt(b) => Right(g(b))
      }

      override def lift[A, B](f: A => B): PartialFunction[A, B] = { case a if a.isInstanceOf[A] => f(a) }

      /**
       * Create a new `F` that takes two inputs, but only modifies the first input
       *
       * Example:
       * {{{
       * scala> import cats.arrow.Arrow
       * scala> val f: PartialFunction[Int, Int] = Arrow[PartialFunction].lift(_ * 2)
       * scala> val fab = Arrow[PartialFunction].first[Int,Int,Int](f)
       * scala> fab((2,3))
       * res0: (Int, Int) = (4,3)
       * }}}
       */
      override def first[A, B, C](fa: PartialFunction[A, B]): PartialFunction[(A, C), (B, C)] = {
        case (a, c) if fa.isDefinedAt(a) => (fa(a), c)
      }

      override def split[A, B, C, D](
        f: PartialFunction[A, B],
        g: PartialFunction[C, D]
      ): PartialFunction[(A, C), (B, D)] = {
        case (a, c) if f.isDefinedAt(a) && g.isDefinedAt(c) => (f(a), g(c))
      }

      override def compose[A, B, C](f: PartialFunction[B, C], g: PartialFunction[A, B]): PartialFunction[A, C] = {
        case a if g.isDefinedAt(a) && f.isDefinedAt(g(a)) => f(g(a))
      }
    }
}
