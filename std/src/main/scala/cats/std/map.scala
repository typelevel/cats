package cats
package std


trait MapInstances {

  implicit def MapShow[A, B](implicit showA: Show[A], showB: Show[B]): Show[Map[A, B]] =
    Show.show[Map[A, B]](m => s"Map(${m.map(a => s"${showA.show(a._1)} -> ${showB.show(a._2)})").mkString(",")})")

  implicit def mapInstance[K]: Traverse[Map[K, ?]] with FlatMap[Map[K, ?]] =
    new Traverse[Map[K, ?]] with FlatMap[Map[K, ?]] {
      def traverse[G[_] : Applicative, A, B](fa: Map[K, A])(f: (A) => G[B]): G[Map[K, B]] = {
        val G = Applicative[G]
        val gba = G.pure(Map.empty[K, B])
        val gbb = fa.foldLeft(gba)((buf, a) => G.map2(buf, f(a._2))({ case(x, y) => x + (a._1 -> y)}))
        G.map(gbb)(_.toMap)
      }

      def flatMap[A, B](fa: Map[K, A])(f: (A) => Map[K, B]): Map[K, B] =
        fa.flatMap { case (_, a) => f(a) }

      def foldLeft[A, B](fa: Map[K, A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b) { case (x, (k, a)) => f(x, a)}

      override def foldRight[A, B](fa: Map[K, A], b: B)(f: (A, B) => B): B =
        fa.foldRight(b) { case ((k, a), z) => f(a, z)}

      def foldLazy[A, B](fa: Map[K, A], b: Lazy[B])(f: A => Fold[B]): Lazy[B] =
        Fold.iterateRight(fa.values, b)(f)
    }
}
