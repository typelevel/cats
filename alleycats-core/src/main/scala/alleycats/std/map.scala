package alleycats
package std

import cats._
import cats.data.Chain
import cats.kernel.instances.StaticMethods.wrapMutableIndexedSeq

object map extends MapInstances

trait MapInstances {

  // toList is inconsistent. See https://github.com/typelevel/cats/issues/1831
  implicit def alleycatsStdInstancesForMap[K]: Traverse[Map[K, *]] =
    new Traverse[Map[K, *]] {

      def traverse[G[_], A, B](fa: Map[K, A])(f: A => G[B])(implicit G: Applicative[G]): G[Map[K, B]] =
        if (fa.isEmpty) G.pure(Map.empty[K, B])
        else
          G.map(Chain.traverseViaChain {
            val as = collection.mutable.ArrayBuffer[(K, A)]()
            as ++= fa
            wrapMutableIndexedSeq(as)
          } {
            case (k, a) => G.map(f(a))((k, _))
          }) { chain => chain.foldLeft(Map.empty[K, B]) { case (m, (k, b)) => m.updated(k, b) } }

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
          else Some(fa.valuesIterator.drop(n).next())
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

  implicit def alleycatsStdMapTraverseFilter[K]: TraverseFilter[Map[K, *]] =
    new TraverseFilter[Map[K, *]] {
      def traverse: Traverse[Map[K, *]] = alleycatsStdInstancesForMap

      def traverseFilter[G[_], A, B](fa: Map[K, A])(f: A => G[Option[B]])(implicit G: Applicative[G]): G[Map[K, B]] =
        if (fa.isEmpty) G.pure(Map.empty[K, B])
        else
          G.map(Chain.traverseFilterViaChain {
            val as = collection.mutable.ArrayBuffer[(K, A)]()
            as ++= fa
            wrapMutableIndexedSeq(as)
          } {
            case (k, a) =>
              G.map(f(a)) { optB =>
                if (optB.isDefined) Some((k, optB.get))
                else None
              }
          }) { chain => chain.foldLeft(Map.empty[K, B]) { case (m, (k, b)) => m.updated(k, b) } }
    }
}
