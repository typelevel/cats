package cats
package std

trait TupleInstances extends Tuple2Instances

sealed trait Tuple2Instances {
  implicit val tuple2Bifoldable: Bifoldable[Tuple2] =
    new Bifoldable[Tuple2] {
      def bifoldLeft[A, B, C](fab: (A, B), c: C)(f: (C, A) => C, g: (C, B) => C): C =
        g(f(c, fab._1), fab._2)

      def bifoldRight[A, B, C](fab: (A, B), c: Eval[C])(f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C]): Eval[C] =
        g(fab._2, f(fab._1, c))
    }
}
