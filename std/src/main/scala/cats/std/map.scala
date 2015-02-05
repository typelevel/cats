package cats
package std

trait MapInstances {

  implicit def mapInstance[K]: Traverse[({type F[V] = Map[K,V]})#F] with FlatMap[({type F[V] = Map[K,V]})#F] =
    new Traverse[({type F[V] = Map[K,V]})#F] with FlatMap[({type F[V] = Map[K,V]})#F] {

      override def traverse[G[_] : Applicative, A, B](fa: Map[K, A])(f: (A) => G[B]) = {
        val G = Applicative[G]
        val gba = G.pure(Map.empty[K, B])
        val gbb = fa.foldLeft(gba)( { case(buf, (k, v)) => G.map2(buf, f(v))({ case(a, b) => a + (k -> b)})} )
        G.map(gbb)(_.toMap)
      }

      override def flatMap[A, B](fa: Map[K, A])(f: (A) => Map[K, B]) = {
        fa.flatMap( {case (_, v) => f(v)} )
      }

      override def foldLeft[A, B](fa: Map[K, A], b: B)(f: (B, A) => B) = {
        fa.foldLeft(b)( { case (b, (_, v)) => f(b, v)} )
      }

      override def foldRight[A, B](fa: Map[K, A], b: B)(f: (A, B) => B) = {
        fa.foldRight(b)( { case ((_, v), b) => f(v, b) })
      }

      override def foldRight[A, B](fa: Map[K, A], b: Lazy[B])(f: (A, Lazy[B]) => B) = {
        list.listInstance.foldRight(fa.values.toList, b)(f)
      }
    }

}

