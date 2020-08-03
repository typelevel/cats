package cats.instances

import cats._
import cats.data.{Chain, Ior}
import cats.kernel.{CommutativeMonoid, CommutativeSemigroup}
import cats.kernel.instances.StaticMethods.wrapMutableIndexedSeq

import scala.annotation.tailrec
import scala.collection.immutable.SortedMap

trait SortedMapInstances extends SortedMapInstances2 {

  @deprecated("Use cats.kernel.instances.sortedMap.catsKernelStdHashForSortedMap", "2.0.0-RC2")
  def catsStdHashForSortedMap[K: Hash: Order, V: Hash]: Hash[SortedMap[K, V]] =
    cats.kernel.instances.sortedMap.catsKernelStdHashForSortedMap[K, V]

  @deprecated("Use cats.kernel.instances.sortedMap.catsKernelStdCommutativeMonoidForSortedMap", "2.0.0-RC2")
  def catsStdCommutativeMonoidForSortedMap[K: Order, V: CommutativeSemigroup] =
    cats.kernel.instances.sortedMap.catsKernelStdCommutativeMonoidForSortedMap[K, V]

  implicit def catsStdShowForSortedMap[A, B](implicit showA: Show[A], showB: Show[B]): Show[SortedMap[A, B]] =
    _.iterator
      .map { case (a, b) => showA.show(a) + " -> " + showB.show(b) }
      .mkString("SortedMap(", ", ", ")")

  @deprecated("Use catsStdShowForSortedMap override without Order", "2.2.0-M3")
  implicit def catsStdShowForSortedMap[A, B](orderA: Order[A], showA: Show[A], showB: Show[B]): Show[SortedMap[A, B]] =
    catsStdShowForSortedMap(showA, showB)

  // scalastyle:off method.length
  implicit def catsStdInstancesForSortedMap[K]
    : Traverse[SortedMap[K, *]] with FlatMap[SortedMap[K, *]] with Align[SortedMap[K, *]] =
    new Traverse[SortedMap[K, *]] with FlatMap[SortedMap[K, *]] with Align[SortedMap[K, *]] {

      def traverse[G[_], A, B](fa: SortedMap[K, A])(f: A => G[B])(implicit G: Applicative[G]): G[SortedMap[K, B]] = {
        implicit val ordering: Ordering[K] = fa.ordering
        if (fa.isEmpty) G.pure(SortedMap.empty[K, B])
        else
          G.map(Chain.traverseViaChain {
            val as = collection.mutable.ArrayBuffer[(K, A)]()
            as ++= fa
            wrapMutableIndexedSeq(as)
          } {
            case (k, a) => G.map(f(a))((k, _))
          }) { chain => chain.foldLeft(SortedMap.empty[K, B]) { case (m, (k, b)) => m.updated(k, b) } }
      }

      def flatMap[A, B](fa: SortedMap[K, A])(f: A => SortedMap[K, B]): SortedMap[K, B] = {
        implicit val ordering: Ordering[K] = fa.ordering
        fa.flatMap { case (k, a) => f(a).get(k).map((k, _)) }
      }

      override def map[A, B](fa: SortedMap[K, A])(f: A => B): SortedMap[K, B] = {
        implicit val ordering: Ordering[K] = fa.ordering
        fa.map { case (k, a) => (k, f(a)) }
      }

      override def map2Eval[A, B, Z](
        fa: SortedMap[K, A],
        fb: Eval[SortedMap[K, B]]
      )(f: (A, B) => Z): Eval[SortedMap[K, Z]] =
        if (fa.isEmpty) Eval.now(SortedMap.empty(fa.ordering)) // no need to evaluate fb
        else fb.map(fb => map2(fa, fb)(f))

      override def ap2[A, B, Z](f: SortedMap[K, (A, B) => Z])(
        fa: SortedMap[K, A],
        fb: SortedMap[K, B]
      ): SortedMap[K, Z] = {
        implicit val ordering: Ordering[K] = f.ordering
        f.flatMap {
          case (k, f) =>
            for { a <- fa.get(k); b <- fb.get(k) } yield (k, f(a, b))
        }
      }

      def foldLeft[A, B](fa: SortedMap[K, A], b: B)(f: (B, A) => B): B =
        fa.valuesIterator.foldLeft(b)(f)

      def foldRight[A, B](fa: SortedMap[K, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        Foldable.iterateRight(fa.values, lb)(f)

      def tailRecM[A, B](a: A)(f: A => SortedMap[K, Either[A, B]]): SortedMap[K, B] = {
        val fa = f(a)
        var bldr = SortedMap.newBuilder[K, B](fa.ordering)

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

        fa.foreach { case (k, a) => descend(k, a) }
        bldr.result()
      }

      override def size[A](fa: SortedMap[K, A]): Long = fa.size.toLong

      override def get[A](fa: SortedMap[K, A])(idx: Long): Option[A] =
        if (idx < 0L || Int.MaxValue < idx) None
        else {
          val n = idx.toInt
          if (n >= fa.size) None
          else Some(fa.valuesIterator.drop(n).next())
        }

      override def isEmpty[A](fa: SortedMap[K, A]): Boolean = fa.isEmpty

      override def fold[A](fa: SortedMap[K, A])(implicit A: Monoid[A]): A =
        A.combineAll(fa.values)

      override def toList[A](fa: SortedMap[K, A]): List[A] = fa.values.toList

      override def collectFirst[A, B](fa: SortedMap[K, A])(pf: PartialFunction[A, B]): Option[B] =
        fa.collectFirst(new PartialFunction[(K, A), B] {
          override def isDefinedAt(x: (K, A)): Boolean = pf.isDefinedAt(x._2)
          override def apply(v1: (K, A)): B = pf(v1._2)
        })

      override def collectFirstSome[A, B](fa: SortedMap[K, A])(f: A => Option[B]): Option[B] =
        collectFirst(fa)(Function.unlift(f))

      def functor: Functor[SortedMap[K, *]] = this

      def align[A, B](fa: SortedMap[K, A], fb: SortedMap[K, B]): SortedMap[K, Ior[A, B]] =
        alignWith(fa, fb)(identity)

      override def alignWith[A, B, C](fa: SortedMap[K, A], fb: SortedMap[K, B])(f: Ior[A, B] => C): SortedMap[K, C] = {
        val keys = fa.keySet ++ fb.keySet
        val builder = SortedMap.newBuilder[K, C](fa.ordering)
        builder.sizeHint(keys.size)
        keys
          .foldLeft(builder) { (builder, k) =>
            (fa.get(k), fb.get(k)) match {
              case (Some(a), Some(b)) => builder += k -> f(Ior.both(a, b))
              case (Some(a), None)    => builder += k -> f(Ior.left(a))
              case (None, Some(b))    => builder += k -> f(Ior.right(b))
              case (None, None)       => ??? // should not happen
            }
          }
          .result()
      }
    }

  @deprecated("Use catsStdInstancesForSortedMap override without Order", "2.2.0-M3")
  def catsStdInstancesForSortedMap[K](
    orderK: Order[K]
  ): Traverse[SortedMap[K, *]] with FlatMap[SortedMap[K, *]] with Align[SortedMap[K, *]] =
    catsStdInstancesForSortedMap[K]
}

private[instances] trait SortedMapInstances1 {
  @deprecated("Use cats.kernel.instances.sortedMap.catsKernelStdEqForSortedMap", "2.0.0-RC2")
  def catsStdEqForSortedMap[K: Order, V: Eq]: Eq[SortedMap[K, V]] =
    new SortedMapEq[K, V]
}

private[instances] trait SortedMapInstances2 extends SortedMapInstances1 {
  @deprecated("Use cats.kernel.instances.sortedMap.catsKernelStdMonoidForSortedMap", "2.0.0-RC2")
  def catsStdMonoidForSortedMap[K: Order, V: Semigroup]: Monoid[SortedMap[K, V]] =
    new SortedMapMonoid[K, V]
}

@deprecated("Use cats.kernel.instances.SortedMapHash", "2.0.0-RC2")
class SortedMapHash[K, V](implicit V: Hash[V], O: Order[K], K: Hash[K])
    extends SortedMapEq[K, V]
    with Hash[SortedMap[K, V]] {
  private[this] val underlying: Hash[SortedMap[K, V]] = new cats.kernel.instances.SortedMapHash[K, V]
  def hash(x: SortedMap[K, V]): Int = underlying.hash(x)
}

@deprecated("Use cats.kernel.instances.SortedMapEq", "2.0.0-RC2")
class SortedMapEq[K, V](implicit V: Eq[V], O: Order[K]) extends cats.kernel.instances.SortedMapEq[K, V]

@deprecated("Use cats.kernel.instances.SortedMapCommutativeMonoid", "2.0.0-RC2")
class SortedMapCommutativeMonoid[K, V](implicit V: CommutativeSemigroup[V], O: Order[K])
    extends SortedMapMonoid[K, V]
    with CommutativeMonoid[SortedMap[K, V]] {
  private[this] val underlying: CommutativeMonoid[SortedMap[K, V]] =
    new cats.kernel.instances.SortedMapCommutativeMonoid[K, V]
}

@deprecated("Use cats.kernel.instances.SortedMapMonoid", "2.0.0-RC2")
class SortedMapMonoid[K, V](implicit V: Semigroup[V], O: Order[K]) extends cats.kernel.instances.SortedMapMonoid[K, V]

private[instances] trait SortedMapInstancesBinCompat0 {
  implicit def catsStdTraverseFilterForSortedMap[K]: TraverseFilter[SortedMap[K, *]] =
    new TraverseFilter[SortedMap[K, *]] {
      val traverse: Traverse[SortedMap[K, *]] = cats.instances.sortedMap.catsStdInstancesForSortedMap[K]

      override def traverseFilter[G[_], A, B](
        fa: SortedMap[K, A]
      )(f: A => G[Option[B]])(implicit G: Applicative[G]): G[SortedMap[K, B]] = {
        implicit val ordering: Ordering[K] = fa.ordering
        if (fa.isEmpty) G.pure(SortedMap.empty[K, B])
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
          }) { chain => chain.foldLeft(SortedMap.empty[K, B]) { case (m, (k, b)) => m.updated(k, b) } }
      }

      override def mapFilter[A, B](fa: SortedMap[K, A])(f: A => Option[B]): SortedMap[K, B] = {
        implicit val ordering: Ordering[K] = fa.ordering
        fa.collect(scala.Function.unlift((t: (K, A)) => f(t._2).map(t._1 -> _)))
      }

      override def collect[A, B](fa: SortedMap[K, A])(f: PartialFunction[A, B]): SortedMap[K, B] = {
        implicit val ordering: Ordering[K] = fa.ordering
        fa.collect(scala.Function.unlift((t: (K, A)) => f.lift(t._2).map(t._1 -> _)))
      }

      override def flattenOption[A](fa: SortedMap[K, Option[A]]): SortedMap[K, A] = {
        implicit val ordering: Ordering[K] = fa.ordering
        fa.collect(scala.Function.unlift((t: (K, Option[A])) => t._2.map(t._1 -> _)))
      }

      override def filter[A](fa: SortedMap[K, A])(f: A => Boolean): SortedMap[K, A] =
        fa.filter { case (_, v) => f(v) }

      override def filterNot[A](fa: SortedMap[K, A])(f: A => Boolean): SortedMap[K, A] =
        fa.filterNot { case (_, v) => f(v) }

      override def filterA[G[_], A](
        fa: SortedMap[K, A]
      )(f: A => G[Boolean])(implicit G: Applicative[G]): G[SortedMap[K, A]] =
        traverseFilter(fa)(a => G.map(f(a))(if (_) Some(a) else None))
    }

  @deprecated("Use catsStdTraverseFilterForSortedMap override without Order", "2.2.0-M3")
  def catsStdTraverseFilterForSortedMap[K](orderK: Order[K]): TraverseFilter[SortedMap[K, *]] =
    catsStdTraverseFilterForSortedMap[K]
}

private[instances] trait SortedMapInstancesBinCompat1 {
  implicit def catsStdSemigroupKForSortedMap[K]: SemigroupK[SortedMap[K, *]] =
    new SemigroupK[SortedMap[K, *]] {
      override def combineK[A](x: SortedMap[K, A], y: SortedMap[K, A]): SortedMap[K, A] = x ++ y
    }

  implicit def catsStdMonoidKForSortedMap[K: Order]: MonoidK[SortedMap[K, *]] =
    new MonoidK[SortedMap[K, *]] {
      override def empty[A]: SortedMap[K, A] = SortedMap.empty[K, A](Order[K].toOrdering)
      override def combineK[A](x: SortedMap[K, A], y: SortedMap[K, A]): SortedMap[K, A] = x ++ y
    }
}

private[instances] trait SortedMapInstancesBinCompat2 extends cats.kernel.instances.SortedMapInstances
