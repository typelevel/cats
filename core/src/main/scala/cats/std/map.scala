package cats
package std

trait MapInstances extends cats.kernel.std.MapInstances {

  implicit def MapShow[A, B](implicit showA: Show[A], showB: Show[B]): Show[Map[A, B]] =
    Show.show[Map[A, B]] { m =>
      val body = m.map { case (a, b) =>
        s"${showA.show(a)} -> ${showB.show(b)})"
      }.mkString(",")
      s"Map($body)"
    }

  implicit def mapInstance[K]: Traverse[Map[K, ?]] with FlatMap[Map[K, ?]] =
    new Traverse[Map[K, ?]] with FlatMap[Map[K, ?]] {

      def traverse[G[_] : Applicative, A, B](fa: Map[K, A])(f: (A) => G[B]): G[Map[K, B]] = {
        val G = Applicative[G]
        val gba = G.pure(Map.empty[K, B])
        val gbb = fa.foldLeft(gba) { (buf, a) =>
          G.map2(buf, f(a._2))({ case(x, y) => x + (a._1 -> y)})
        }
        G.map(gbb)(_.toMap)
      }

      override def map[A, B](fa: Map[K, A])(f: A => B): Map[K, B] =
        fa.map { case (k, a) => (k, f(a)) }

      override def map2[A, B, Z](fa: Map[K, A], fb: Map[K, B])(f: (A, B) => Z): Map[K, Z] =
        fa.flatMap { case (k, a) => fb.get(k).map(b => (k, f(a, b))) }

      override def ap[A, B](ff: Map[K, A => B])(fa: Map[K, A]): Map[K, B] =
        fa.flatMap { case (k, a) => ff.get(k).map(f => (k, f(a))) }

      override def ap2[A, B, Z](f: Map[K, (A, B) => Z])(fa: Map[K, A], fb: Map[K, B]): Map[K, Z] =
        f.flatMap { case (k, f) =>
          for { a <- fa.get(k); b <- fb.get(k) } yield (k, f(a, b))
        }

      def flatMap[A, B](fa: Map[K, A])(f: (A) => Map[K, B]): Map[K, B] =
        fa.flatMap { case (k, a) => f(a).get(k).map((k, _)) }

      def foldLeft[A, B](fa: Map[K, A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b) { case (x, (k, a)) => f(x, a)}

      def foldRight[A, B](fa: Map[K, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        Foldable.iterateRight(fa.values.iterator, lb)(f)

      override def isEmpty[A](fa: Map[K, A]): Boolean = fa.isEmpty
    }
}
