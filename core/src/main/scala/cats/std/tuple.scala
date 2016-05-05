package cats
package std

trait TupleInstances extends Tuple2Instances with cats.kernel.std.TupleInstances

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

  implicit def tuple2Show[A, B](implicit aShow: Show[A], bShow: Show[B]): Show[(A, B)] = new Show[(A, B)] {
    override def show(f: (A, B)): String = {
      s"(${aShow.show(f._1)},${bShow.show(f._2)})"
    }
  }
}
