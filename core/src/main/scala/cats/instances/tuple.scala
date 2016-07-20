package cats
package instances

trait TupleInstances extends Tuple2Instances with cats.kernel.instances.TupleInstances

sealed trait Tuple2Instances {
  implicit val catsStdBitraverseForTuple2: Bitraverse[Tuple2] =
    new Bitraverse[Tuple2] {
      def bitraverse[G[_]: Applicative, A, B, C, D](fab: (A, B))(f: A => G[C], g: B => G[D]): G[(C, D)] =
        Applicative[G].tuple2(f(fab._1), g(fab._2))

      def bifoldLeft[A, B, C](fab: (A, B), c: C)(f: (C, A) => C, g: (C, B) => C): C =
        g(f(c, fab._1), fab._2)

      def bifoldRight[A, B, C](fab: (A, B), c: Eval[C])(f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C]): Eval[C] =
        g(fab._2, f(fab._1, c))
    }

  implicit def catsStdShowForTuple2[A, B](implicit aShow: Show[A], bShow: Show[B]): Show[(A, B)] = new Show[(A, B)] {
    override def show(f: (A, B)): String = {
      s"(${aShow.show(f._1)},${bShow.show(f._2)})"
    }
  }

  implicit def catsStdInstancesForTuple2[X]: Traverse[(X, ?)] with Comonad[(X, ?)] =
    new Traverse[(X, ?)] with Comonad[(X, ?)] {
      def traverse[G[_], A, B](fa: (X, A))(f: A => G[B])(implicit G: Applicative[G]): G[(X, B)] =
        G.map(f(fa._2))((fa._1, _))

      def foldLeft[A, B](fa: (X, A), b: B)(f: (B, A) => B): B = f(b, fa._2)

      def foldRight[A, B](fa: (X, A), lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = f(fa._2, lb)

      override def map[A, B](fa: (X, A))(f: A => B): (X, B) = (fa._1, f(fa._2))

      def coflatMap[A, B](fa: (X, A))(f: ((X, A)) => B): (X, B) = (fa._1, f(fa))

      def extract[A](fa: (X, A)): A = fa._2

      override def coflatten[A](fa: (X, A)): (X, (X, A)) = (fa._1, fa)
    }
}
