package cats
package std

trait TupleInstances extends Tuple2Instances

sealed trait Tuple2Instances {
  implicit val tuple2Bitraverse: Bitraverse[Tuple2] =
    new Bitraverse[Tuple2] {
      def bitraverse[G[_]: Applicative, A, B, C, D](fab: (A, B))(f: A => G[C], g: B => G[D]): G[(C, D)] =
        Applicative[G].tuple2(f(fab._1), g(fab._2))

      def bifoldLeft[A, B, C](fab: (A, B), c: C)(f: (C, A) => C, g: (C, B) => C): C =
        g(f(c, fab._1), fab._2)

      def bifoldRight[A, B, C](fab: (A, B), c: Eval[C])(f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C]): Eval[C] =
        g(fab._2, f(fab._1, c))
    }
}
