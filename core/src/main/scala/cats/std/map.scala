package cats
package std

import scala.collection.mutable

import cats.syntax.eq._

trait MapInstances {

  implicit def MapEq[A, B: Eq]: Eq[Map[A, B]] =
    new Eq[Map[A, B]] {
      def eqv(lhs: Map[A, B], rhs: Map[A, B]): Boolean = {
        def checkKeys: Boolean =
          lhs.forall { case (k, v1) =>
            rhs.get(k) match {
              case Some(v2) => v1 === v2
              case None => false
            }
          }
        (lhs eq rhs) || (lhs.size == rhs.size && checkKeys)
      }
    }

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

      override def ap[A, B](fa: Map[K, A])(ff: Map[K, A => B]): Map[K, B] =
        fa.flatMap { case (k, a) => ff.get(k).map(f => (k, f(a))) }

      override def ap2[A, B, Z](fa: Map[K, A], fb: Map[K, B])(f: Map[K, (A, B) => Z]): Map[K, Z] =
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

  implicit def mapMonoid[K, V: Semigroup]: MapMonoid[K, V] =
    new MapMonoid[K, V]

  implicit def mapGroup[K, V: Group]: MapGroup[K, V] =
    new MapGroup[K, V]
}

class MapMonoid[K, V](implicit V: Semigroup[V]) extends Monoid[Map[K, V]]  {
  def empty: Map[K, V] = Map.empty

  def combine(x: Map[K, V], y: Map[K, V]): Map[K, V] =
    MapMethods.addMap(x, y)(V.combine)
}

class MapGroup[K, V](implicit V: Group[V]) extends MapMonoid[K, V] with Group[Map[K, V]] {
  def inverse(x: Map[K, V]): Map[K, V] =
    x.map { case (k, v) => (k, V.inverse(v)) }

  override def remove(x: Map[K, V], y: Map[K, V]): Map[K, V] =
    MapMethods.subtractMap(x, y)(V.remove)(V.inverse)
}

object MapMethods {
  def initMutableMap[K, V](m: Map[K, V]): mutable.Map[K, V] = {
    val result = mutable.Map.empty[K, V]
    m.foreach { case (k, v) => result(k) = v }
    result
  }

  def wrapMutableMap[K, V](m: mutable.Map[K, V]): Map[K, V] =
    new WrappedMutableMap(m)

  private[cats] class WrappedMutableMap[K, V](m: mutable.Map[K, V]) extends Map[K, V] {
    override def size: Int = m.size
    def get(k: K): Option[V] = m.get(k)
    def iterator: Iterator[(K, V)] = m.iterator
    def +[V2 >: V](kv: (K, V2)): Map[K, V2] = m.toMap + kv
    def -(key: K): Map[K, V] = m.toMap - key
  }

  def addMap[K, V](x: Map[K, V], y: Map[K, V])(f: (V, V) => V): Map[K, V] = {
    val (small, big, g) =
      if (x.size <= y.size) (x, y, f)
      else (y, x, (v1: V, v2: V) => f(v2, v1))

    val m = initMutableMap(big)
    small.foreach { case (k, v1) =>
      m(k) = m.get(k) match {
        case Some(v2) => g(v1, v2)
        case None => v1
      }
    }
    wrapMutableMap(m)
  }

  def subtractMap[K, V](x: Map[K, V], y: Map[K, V])(subtract: (V, V) => V)(negate: V => V): Map[K, V] = {
    // even if x is smaller, we'd need to call map/foreach on y to
    // negate all its values, so this is just as fast or faster.
    val m = initMutableMap(x)
    y.foreach { case (k, v2) =>
      m(k) = m.get(k) match {
        case Some(v1) => subtract(v1, v2)
        case None => negate(v2)
      }
    }
    wrapMutableMap(m)
  }
}
