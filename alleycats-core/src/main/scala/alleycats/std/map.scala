package alleycats
package std

import cats._

object map extends MapInstances with MapInstancesBinCompat0

trait MapInstances {

  // toList is inconsistent. See https://github.com/typelevel/cats/issues/1831
  implicit def alleycatsStdInstancesForMap[K]: Traverse[Map[K, ?]] =
    new Traverse[Map[K, ?]] {

      def traverse[G[_], A, B](fa: Map[K, A])(f: A => G[B])(implicit G: Applicative[G]): G[Map[K, B]] = {
        val gba: Eval[G[Map[K, B]]] = Always(G.pure(Map.empty))
        val gbb = Foldable
          .iterateRight(fa, gba) { (kv, lbuf) =>
            G.map2Eval(f(kv._2), lbuf)({ (b, buf) =>
              buf + (kv._1 -> b)
            })
          }
          .value
        G.map(gbb)(_.toMap)
      }

      override def map[A, B](fa: Map[K, A])(f: A => B): Map[K, B] =
        fa.map { case (k, a) => (k, f(a)) }

      def foldLeft[A, B](fa: Map[K, A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b) { case (x, (k, a)) => f(x, a) }

      def foldRight[A, B](fa: Map[K, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        Foldable.iterateRight(fa.values, lb)(f)

      override def size[A](fa: Map[K, A]): Long = fa.size.toLong

      override def get[A](fa: Map[K, A])(idx: Long): Option[A] =
        if (idx < 0L || Int.MaxValue < idx) None
        else {
          val n = idx.toInt
          if (n >= fa.size) None
          else Some(fa.valuesIterator.drop(n).next)
        }

      override def isEmpty[A](fa: Map[K, A]): Boolean = fa.isEmpty

      override def fold[A](fa: Map[K, A])(implicit A: Monoid[A]): A =
        A.combineAll(fa.values)

      override def toList[A](fa: Map[K, A]): List[A] = fa.values.toList

      override def collectFirst[A, B](fa: Map[K, A])(pf: PartialFunction[A, B]): Option[B] =
        fa.collectFirst(new PartialFunction[(K, A), B] {
          override def isDefinedAt(x: (K, A)) = pf.isDefinedAt(x._2)
          override def apply(v1: (K, A)) = pf(v1._2)
        })

      override def collectFirstSome[A, B](fa: Map[K, A])(f: A => Option[B]): Option[B] =
        collectFirst(fa)(Function.unlift(f))
    }
}

trait MapInstancesBinCompat0 extends MapInstances {
  implicit def mapTraverseFilter[K]: TraverseFilter[Map[K, ?]] =
    new TraverseFilter[Map[K, ?]] {
      def traverse: Traverse[Map[K, ?]] = alleycatsStdInstancesForMap

      def traverseFilter[G[_], A, B](fa: Map[K, A])(f: A => G[Option[B]])(implicit G: Applicative[G]): G[Map[K, B]] = {
        val gba: Eval[G[Map[K, B]]] = Always(G.pure(Map.empty))
        Foldable
          .iterateRight(fa, gba) { (kv, lbuf) =>
            G.map2Eval(f(kv._2), lbuf)({ (ob, buf) =>
              ob.fold(buf)(b => buf + (kv._1 -> b))
            })
          }
          .value
      }
    }
}
