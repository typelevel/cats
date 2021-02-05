package cats
package syntax

import cats.data.{NonEmptyChain, NonEmptyList}
import cats.kernel.Semigroup
import scala.collection.immutable.{SortedMap, TreeMap}

trait ListSyntax {
  implicit final def catsSyntaxList[A](la: List[A]): ListOps[A] = new ListOps(la)
}

final class ListOps[A](private val la: List[A]) extends AnyVal {

  /**
   * Returns an Option of NonEmptyList from a List
   *
   * Example:
   * {{{
   * scala> import cats.data.NonEmptyList
   * scala> import cats.implicits._
   *
   * scala> val result1: List[Int] = List(1, 2)
   * scala> result1.toNel
   * res0: Option[NonEmptyList[Int]] = Some(NonEmptyList(1, 2))
   *
   * scala> val result2: List[Int] = List.empty[Int]
   * scala> result2.toNel
   * res1: Option[NonEmptyList[Int]] = None
   * }}}
   */
  def toNel: Option[NonEmptyList[A]] = NonEmptyList.fromList(la)

  /**
   * Groups elements inside this `List` according to the `Order` of the keys
   * produced by the given key function.
   *
   * {{{
   * scala> import scala.collection.immutable.SortedMap
   * scala> import cats.data.NonEmptyList
   * scala> import cats.implicits._
   * scala> val list = List(12, -2, 3, -5)
   * scala> val expectedResult = SortedMap(false -> NonEmptyList.of(-2, -5), true -> NonEmptyList.of(12, 3))
   * scala> list.groupByNel(_ >= 0) === expectedResult
   * res0: Boolean = true
   * }}}
   */
  def groupByNel[K](key: A => K)(implicit K: Order[K]): SortedMap[K, NonEmptyList[A]] = {
    implicit val ordering: Ordering[K] = K.toOrdering
    toNel.fold(SortedMap.empty[K, NonEmptyList[A]])(_.groupBy(key))
  }

  /**
   * Groups elements inside this `List` according to the `Order` of the keys
   * produced by the given key function.
   * And each element in a group is transformed into a value of type B
   * using the mapping function.
   *
   * {{{
   * scala> import scala.collection.immutable.SortedMap
   * scala> import cats.data.NonEmptyList
   * scala> import cats.implicits._
   * scala> val list = List(12, -2, 3, -5)
   * scala> val expectedResult = SortedMap(false -> NonEmptyList.of("-2", "-5"), true -> NonEmptyList.of("12", "3"))
   * scala> list.groupMapNel(_ >= 0)(_.toString) === expectedResult
   * res0: Boolean = true
   * }}}
   */
  def groupMapNel[K, B](key: A => K)(f: A => B)(implicit K: Order[K]): SortedMap[K, NonEmptyList[B]] = {
    implicit val ordering: Ordering[K] = K.toOrdering
    toNel.fold(SortedMap.empty[K, NonEmptyList[B]])(_.groupMap(key)(f))
  }

  /**
   * Groups elements inside this `List` according to the `Order` of the keys
   * produced by the given key function.
   * Then each element in a group is transformed into a value of type B
   * using the mapping function.
   * And finally they are all reduced into a single value
   * using their `Semigroup`.
   *
   * {{{
   * scala> import scala.collection.immutable.SortedMap
   * scala> import cats.implicits._
   * scala> val list = List("Hello", "World", "Goodbye", "World")
   * scala> val expectedResult = SortedMap("goodbye" -> 1, "hello" -> 1, "world" -> 2)
   * scala> list.groupMapReduce(_.trim.toLowerCase)(_ => 1) === expectedResult
   * res0: Boolean = true
   * }}}
   */
  def groupMapReduce[K, B](key: A => K)(f: A => B)(implicit K: Order[K], B: Semigroup[B]): SortedMap[K, B] =
    groupMapReduceWith(key)(f)(B.combine)

  /**
   * Groups elements inside this `List` according to the `Order` of the keys
   * produced by the given key function.
   * Then each element in a group is transformed into a value of type B
   * using the mapping function.
   * And finally they are all reduced into a single value
   * using the provided combine function.
   *
   * {{{
   * scala> import scala.collection.immutable.SortedMap
   * scala> import cats.implicits._
   * scala> val list = List("Hello", "World", "Goodbye", "World")
   * scala> val expectedResult = SortedMap("goodbye" -> 1, "hello" -> 1, "world" -> 2)
   * scala> list.groupMapReduceWith(_.trim.toLowerCase)(_ => 1)(_ + _) === expectedResult
   * res0: Boolean = true
   * }}}
   */
  def groupMapReduceWith[K, B](key: A => K)(f: A => B)(combine: (B, B) => B)(implicit K: Order[K]): SortedMap[K, B] = {
    implicit val ordering: Ordering[K] = K.toOrdering
    var m = TreeMap.empty[K, B]

    for (elem <- la) {
      val k = key(elem)

      m.get(k) match {
        case Some(b) => m = m.updated(key = k, value = combine(b, f(elem)))
        case None    => m += (k -> f(elem))
      }
    }

    m
  }

  /**
   * Groups elements inside this `List` according to the `Order` of the keys
   * produced by the given mapping monadic function.
   *
   * {{{
   * scala> import cats.data.NonEmptyList
   * scala> import scala.collection.immutable.SortedMap
   * scala> import cats.implicits._
   *
   * scala> val list = List(12, -2, 3, -5)
   *
   * scala> val expectedResult = Option(SortedMap(false -> NonEmptyList.of(-2, -5), true -> NonEmptyList.of(12, 3)))
   *
   * scala> list.groupByNelA(num => Option(0).map(num >= _)) === expectedResult
   * res0: Boolean = true
   * }}}
   */
  def groupByNelA[F[_], B](f: A => F[B])(implicit F: Applicative[F], B: Order[B]): F[SortedMap[B, NonEmptyList[A]]] = {
    implicit val ordering: Ordering[B] = B.toOrdering
    val functor = Functor[SortedMap[B, *]]

    toNel.fold(F.pure(SortedMap.empty[B, NonEmptyList[A]]))(nel =>
      F.map(nel.traverse(a => F.tupleLeft(f(a), a)))(list => functor.map(list.groupBy(_._2))(_.map(_._1)))
    )
  }

  /**
   * Produces a `NonEmptyList` containing cumulative results of applying the
   * operator going left to right.
   *
   * Example:
   * {{{
   * scala> import cats.data.NonEmptyList
   * scala> import cats.implicits._
   *
   * scala> val result1: List[Int] = List(1, 2)
   * scala> result1.scanLeftNel(100)(_ + _)
   * res0: NonEmptyList[Int] = NonEmptyList(100, 101, 103)
   *
   * scala> val result2: List[Int] = List.empty[Int]
   * scala> result2.scanLeftNel(1)(_ + _)
   * res1: NonEmptyList[Int] = NonEmptyList(1)
   * }}}
   */
  def scanLeftNel[B](b: B)(f: (B, A) => B): NonEmptyList[B] =
    NonEmptyList.fromListUnsafe(la.scanLeft(b)(f))

  /**
   * Produces a `NonEmptyList` containing cumulative results of applying the
   * operator going right to left.
   *
   * Example:
   * {{{
   * scala> import cats.data.NonEmptyList
   * scala> import cats.implicits._
   *
   * scala> val result1: List[Int] = List(1, 2)
   * scala> result1.scanRightNel(100)(_ + _)
   * res0: NonEmptyList[Int] = NonEmptyList(103, 102, 100)
   *
   * scala> val result2: List[Int] = List.empty[Int]
   * scala> result2.scanRightNel(1)(_ + _)
   * res1: NonEmptyList[Int] = NonEmptyList(1)
   * }}}
   */
  def scanRightNel[B](b: B)(f: (A, B) => B): NonEmptyList[B] =
    NonEmptyList.fromListUnsafe(la.scanRight(b)(f))
}

private[syntax] trait ListSyntaxBinCompat0 {
  implicit final def catsSyntaxListBinCompat0[A](la: List[A]): ListOpsBinCompat0[A] = new ListOpsBinCompat0(la)
}

final private[syntax] class ListOpsBinCompat0[A](private val la: List[A]) extends AnyVal {

  /**
   * Returns an Option of NonEmptyChain from a List
   *
   * Example:
   * {{{
   * scala> import cats.data.NonEmptyChain
   * scala> import cats.implicits._
   *
   * scala> val result1: List[Int] = List(1, 2)
   * scala> result1.toNec
   * res0: Option[NonEmptyChain[Int]] = Some(Chain(1, 2))
   *
   * scala> val result2: List[Int] = List.empty[Int]
   * scala> result2.toNec
   * res1: Option[NonEmptyChain[Int]] = None
   * }}}
   */
  def toNec: Option[NonEmptyChain[A]] =
    NonEmptyChain.fromSeq(la)

  /**
   * Groups elements inside this `List` according to the `Order` of the keys
   * produced by the given key function.
   *
   * {{{
   * scala> import scala.collection.immutable.SortedMap
   * scala> import cats.data.NonEmptyChain
   * scala> import cats.implicits._
   * scala> val list = List(12, -2, 3, -5)
   * scala> val expectedResult = SortedMap(false -> NonEmptyChain(-2, -5), true -> NonEmptyChain(12, 3))
   * scala> list.groupByNec(_ >= 0) === expectedResult
   * res0: Boolean = true
   * }}}
   */
  def groupByNec[K](key: A => K)(implicit K: Order[K]): SortedMap[K, NonEmptyChain[A]] = {
    implicit val ordering: Ordering[K] = K.toOrdering
    toNec.fold(SortedMap.empty[K, NonEmptyChain[A]])(_.groupBy(key).toSortedMap)
  }

  /**
   * Groups elements inside this `List` according to the `Order` of the keys
   * produced by the given key function.
   * And each element in a group is transformed into a value of type B
   * using the mapping function.
   *
   * {{{
   * scala> import scala.collection.immutable.SortedMap
   * scala> import cats.data.NonEmptyChain
   * scala> import cats.implicits._
   * scala> val list = List(12, -2, 3, -5)
   * scala> val expectedResult = SortedMap(false -> NonEmptyChain("-2", "-5"), true -> NonEmptyChain("12", "3"))
   * scala> list.groupMapNec(_ >= 0)(_.toString) === expectedResult
   * res0: Boolean = true
   * }}}
   */
  def groupMapNec[K, B](key: A => K)(f: A => B)(implicit K: Order[K]): SortedMap[K, NonEmptyChain[B]] = {
    implicit val ordering: Ordering[K] = K.toOrdering
    toNec.fold(SortedMap.empty[K, NonEmptyChain[B]])(_.groupMap(key)(f).toSortedMap)
  }
}
