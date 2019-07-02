package cats.instances

import cats.{Always, Applicative, Eval, FlatMap, Foldable, Monoid, MonoidK, Order, Show, Traverse, TraverseFilter}
import cats.kernel._
import cats.kernel.instances.StaticMethods

import scala.annotation.tailrec
import scala.collection.immutable.SortedMap

trait SortedMapInstances extends SortedMapInstances2 {

  implicit def catsStdHashForSortedMap[K: Hash: Order, V: Hash]: Hash[SortedMap[K, V]] =
    new SortedMapHash[K, V]

  implicit def catsStdCommutativeMonoidForSortedMap[K: Order, V: CommutativeSemigroup]
    : CommutativeMonoid[SortedMap[K, V]] =
    new SortedMapCommutativeMonoid[K, V]

  implicit def catsStdShowForSortedMap[A: Order, B](implicit showA: Show[A], showB: Show[B]): Show[SortedMap[A, B]] =
    new Show[SortedMap[A, B]] {
      def show(m: SortedMap[A, B]): String =
        m.iterator
          .map { case (a, b) => showA.show(a) + " -> " + showB.show(b) }
          .mkString("SortedMap(", ", ", ")")
    }

  // scalastyle:off method.length
  implicit def catsStdInstancesForSortedMap[K: Order]: Traverse[SortedMap[K, ?]] with FlatMap[SortedMap[K, ?]] =
    new Traverse[SortedMap[K, ?]] with FlatMap[SortedMap[K, ?]] {

      implicit val orderingK: Ordering[K] = Order[K].toOrdering

      def traverse[G[_], A, B](fa: SortedMap[K, A])(f: A => G[B])(implicit G: Applicative[G]): G[SortedMap[K, B]] = {
        val gba: Eval[G[SortedMap[K, B]]] = Always(G.pure(SortedMap.empty(Order[K].toOrdering)))
        Foldable
          .iterateRight(fa, gba) { (kv, lbuf) =>
            G.map2Eval(f(kv._2), lbuf)({ (b, buf) =>
              buf + (kv._1 -> b)
            })
          }
          .value
      }

      def flatMap[A, B](fa: SortedMap[K, A])(f: A => SortedMap[K, B]): SortedMap[K, B] =
        fa.flatMap { case (k, a) => f(a).get(k).map((k, _)) }

      override def map[A, B](fa: SortedMap[K, A])(f: A => B): SortedMap[K, B] =
        fa.map { case (k, a) => (k, f(a)) }

      override def map2Eval[A, B, Z](fa: SortedMap[K, A],
                                     fb: Eval[SortedMap[K, B]])(f: (A, B) => Z): Eval[SortedMap[K, Z]] =
        if (fa.isEmpty) Eval.now(SortedMap.empty(Order[K].toOrdering)) // no need to evaluate fb
        else fb.map(fb => map2(fa, fb)(f))

      override def ap2[A, B, Z](f: SortedMap[K, (A, B) => Z])(fa: SortedMap[K, A],
                                                              fb: SortedMap[K, B]): SortedMap[K, Z] =
        f.flatMap {
          case (k, f) =>
            for { a <- fa.get(k); b <- fb.get(k) } yield (k, f(a, b))
        }

      def foldLeft[A, B](fa: SortedMap[K, A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b) { case (x, (k, a)) => f(x, a) }

      def foldRight[A, B](fa: SortedMap[K, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        Foldable.iterateRight(fa.values, lb)(f)

      def tailRecM[A, B](a: A)(f: A => SortedMap[K, Either[A, B]]): SortedMap[K, B] = {
        val bldr = SortedMap.newBuilder[K, B](Order[K].toOrdering)

        @tailrec def descend(k: K, either: Either[A, B]): Unit =
          either match {
            case Left(a) =>
              f(a).get(k) match {
                case Some(x) => descend(k, x)
                case None    => ()
              }
            case Right(b) =>
              bldr += ((k, b))
              ()
          }

        f(a).foreach { case (k, a) => descend(k, a) }
        bldr.result
      }

      override def size[A](fa: SortedMap[K, A]): Long = fa.size.toLong

      override def get[A](fa: SortedMap[K, A])(idx: Long): Option[A] =
        if (idx < 0L || Int.MaxValue < idx) None
        else {
          val n = idx.toInt
          if (n >= fa.size) None
          else Some(fa.valuesIterator.drop(n).next)
        }

      override def isEmpty[A](fa: SortedMap[K, A]): Boolean = fa.isEmpty

      override def fold[A](fa: SortedMap[K, A])(implicit A: Monoid[A]): A =
        A.combineAll(fa.values)

      override def toList[A](fa: SortedMap[K, A]): List[A] = fa.values.toList

      override def collectFirst[A, B](fa: SortedMap[K, A])(pf: PartialFunction[A, B]): Option[B] =
        fa.collectFirst(new PartialFunction[(K, A), B] {
          override def isDefinedAt(x: (K, A)) = pf.isDefinedAt(x._2)
          override def apply(v1: (K, A)) = pf(v1._2)
        })

      override def collectFirstSome[A, B](fa: SortedMap[K, A])(f: A => Option[B]): Option[B] =
        collectFirst(fa)(Function.unlift(f))
    }

}

trait SortedMapInstances1 {
  implicit def catsStdEqForSortedMap[K: Order, V: Eq]: Eq[SortedMap[K, V]] =
    new SortedMapEq[K, V]
}

trait SortedMapInstances2 extends SortedMapInstances1 {
  implicit def catsStdMonoidForSortedMap[K: Order, V: Semigroup]: Monoid[SortedMap[K, V]] =
    new SortedMapMonoid[K, V]
}

class SortedMapHash[K, V](implicit V: Hash[V], O: Order[K], K: Hash[K])
    extends SortedMapEq[K, V]()(V, O)
    with Hash[SortedMap[K, V]] {
  // adapted from [[scala.util.hashing.MurmurHash3]],
  // but modified standard `Any#hashCode` to `ev.hash`.
  import scala.util.hashing.MurmurHash3._
  def hash(x: SortedMap[K, V]): Int = {
    var a, b, n = 0
    var c = 1
    x.foreach {
      case (k, v) =>
        val h = StaticMethods.product2HashWithPrefix(K.hash(k), V.hash(v), "Tuple2")
        a += h
        b ^= h
        c = StaticMethods.updateUnorderedHashC(c, h)
        n += 1
    }
    var h = mapSeed
    h = mix(h, a)
    h = mix(h, b)
    h = mixLast(h, c)
    finalizeHash(h, n)
  }
}

class SortedMapEq[K, V](implicit V: Eq[V], O: Order[K]) extends Eq[SortedMap[K, V]] {
  def eqv(x: SortedMap[K, V], y: SortedMap[K, V]): Boolean =
    if (x eq y) true
    else
      x.size == y.size && x.forall {
        case (k, v1) =>
          y.get(k) match {
            case Some(v2) => V.eqv(v1, v2)
            case None     => false
          }
      }
}

class SortedMapCommutativeMonoid[K, V](implicit V: CommutativeSemigroup[V], O: Order[K])
    extends SortedMapMonoid[K, V]
    with CommutativeMonoid[SortedMap[K, V]]

class SortedMapMonoid[K, V](implicit V: Semigroup[V], O: Order[K]) extends Monoid[SortedMap[K, V]] {

  def empty: SortedMap[K, V] = SortedMap.empty(O.toOrdering)

  def combine(xs: SortedMap[K, V], ys: SortedMap[K, V]): SortedMap[K, V] =
    if (xs.size <= ys.size) {
      xs.foldLeft(ys) {
        case (my, (k, x)) =>
          my.updated(k, Semigroup.maybeCombine(x, my.get(k)))
      }
    } else {
      ys.foldLeft(xs) {
        case (mx, (k, y)) =>
          mx.updated(k, Semigroup.maybeCombine(mx.get(k), y))
      }
    }

}

trait SortedMapInstancesBinCompat0 {
  implicit def catsStdTraverseFilterForSortedMap[K: Order]: TraverseFilter[SortedMap[K, ?]] =
    new TraverseFilter[SortedMap[K, ?]] {

      implicit val ordering: Ordering[K] = Order[K].toOrdering

      val traverse: Traverse[SortedMap[K, ?]] = cats.instances.sortedMap.catsStdInstancesForSortedMap[K]

      override def traverseFilter[G[_], A, B](
        fa: SortedMap[K, A]
      )(f: A => G[Option[B]])(implicit G: Applicative[G]): G[SortedMap[K, B]] = {
        val gba: Eval[G[SortedMap[K, B]]] = Always(G.pure(SortedMap.empty))
        Foldable
          .iterateRight(fa, gba) { (kv, lbuf) =>
            G.map2Eval(f(kv._2), lbuf)({ (ob, buf) =>
              ob.fold(buf)(b => buf + (kv._1 -> b))
            })
          }
          .value
      }

      override def mapFilter[A, B](fa: SortedMap[K, A])(f: (A) => Option[B]): SortedMap[K, B] =
        fa.collect(scala.Function.unlift(t => f(t._2).map(t._1 -> _)))

      override def collect[A, B](fa: SortedMap[K, A])(f: PartialFunction[A, B]): SortedMap[K, B] =
        fa.collect(scala.Function.unlift(t => f.lift(t._2).map(t._1 -> _)))

      override def flattenOption[A](fa: SortedMap[K, Option[A]]): SortedMap[K, A] =
        fa.collect(scala.Function.unlift(t => t._2.map(t._1 -> _)))

      override def filter[A](fa: SortedMap[K, A])(f: (A) => Boolean): SortedMap[K, A] =
        fa.filter { case (_, v) => f(v) }

      override def filterA[G[_], A](
        fa: SortedMap[K, A]
      )(f: (A) => G[Boolean])(implicit G: Applicative[G]): G[SortedMap[K, A]] =
        traverseFilter(fa)(a => G.map(f(a))(if (_) Some(a) else None))
    }
}

trait SortedMapInstancesBinCompat1 {
  implicit def catsStdMonoidKForSortedMap[K: Order]: MonoidK[SortedMap[K, ?]] = new MonoidK[SortedMap[K, ?]] {
    override def empty[A]: SortedMap[K, A] = SortedMap.empty[K, A](Order[K].toOrdering)

    override def combineK[A](x: SortedMap[K, A], y: SortedMap[K, A]): SortedMap[K, A] = x ++ y
  }
}
