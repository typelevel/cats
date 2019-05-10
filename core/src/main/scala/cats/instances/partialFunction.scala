package cats
package instances

import cats.arrow.ArrowChoice

trait PartialFunctionInstances {
  implicit val catsStdInstancesForPartialFunction = new ArrowChoice[PartialFunction] {
    def choose[A, B, C, D](
      f: PartialFunction[A, C]
    )(g: PartialFunction[B, D]): PartialFunction[Either[A, B], Either[C, D]] = {
      case Left(a) if f.isDefinedAt(a)  => Left(f(a))
      case Right(b) if g.isDefinedAt(b) => Right(g(b))
    }

    def lift[A, B](f: A => B): PartialFunction[A, B] = {
      case a => f(a)
    }

    def compose[A, B, C](f: PartialFunction[B, C], g: PartialFunction[A, B]): PartialFunction[A, C] = {
      case a if g.isDefinedAt(a) && f.isDefinedAt(g(a)) => f(g(a))
    }

    def first[A, B, C](fa: PartialFunction[A, B]): PartialFunction[(A, C), (B, C)] = {
      case (a, c) if fa.isDefinedAt(a) => (fa(a), c)
    }
  }
}
