package cats
package std

import cats.functor.Bifunctor

trait TupleInstances extends Tuple2Instances

sealed trait Tuple2Instances {
  implicit val tuple2Bifunctor: Bifunctor[Tuple2] =
    new Bifunctor[Tuple2] {
      def bimap[A, B, C, D](fab: (A, B))(f: A => C, g: B => D): (C, D) =
        (f(fab._1), g(fab._2))
    }
}
