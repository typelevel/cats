package cats
package data

import NonEmptyLazyList.create
import kernel.PartialOrder
import instances.lazyList._

import scala.collection.immutable.{SortedMap, TreeMap, TreeSet}
import scala.collection.mutable

object NonEmptyLazyList extends NonEmptyLazyListInstances {

  // The following 3 types are components of a technique to
  // create a no-boxing newtype. It's coped from the
  // newtypes lib by @alexknvl
  // For more detail see https://github.com/alexknvl/newtypes
  private[data] type Base
  private[data] trait Tag extends Any
  /* aliased in data package as NonEmptyLazyList */
  type Type[+A] <: Base with Tag

  private[data] def create[A](s: LazyList[A]): Type[A] =
    s.asInstanceOf[Type[A]]

  private[data] def unwrap[A](s: Type[A]): LazyList[A] =
    s.asInstanceOf[LazyList[A]]

  def fromLazyList[A](as: LazyList[A]): Option[NonEmptyLazyList[A]] =
    if (as.nonEmpty) Option(create(as)) else None

  def fromLazyListUnsafe[A](ll: LazyList[A]): NonEmptyLazyList[A] =
    if (ll.nonEmpty) create(ll)
    else throw new IllegalArgumentException("Cannot create NonEmptyLazyList from empty LazyList")

  def fromNonEmptyList[A](as: NonEmptyList[A]): NonEmptyLazyList[A] =
    create(LazyList.from(as.toList))

  def fromNonEmptyVector[A](as: NonEmptyVector[A]): NonEmptyLazyList[A] =
    create(LazyList.from(as.toVector))

  def fromSeq[A](as: Seq[A]): Option[NonEmptyLazyList[A]] =
    if (as.nonEmpty) Option(create(LazyList.from(as))) else None

  def fromLazyListPrepend[A](a: A, ca: LazyList[A]): NonEmptyLazyList[A] =
    create(a +: ca)

  def fromLazyListAppend[A](ca: LazyList[A], a: A): NonEmptyLazyList[A] =
    create(ca :+ a)

  def apply[A](a: => A, as: A*): NonEmptyLazyList[A] =
    create(LazyList.concat(LazyList(a), LazyList.from(as)))

  implicit def catsNonEmptyLazyListOps[A](value: NonEmptyLazyList[A]): NonEmptyLazyListOps[A] =
    new NonEmptyLazyListOps(value)
}

class NonEmptyLazyListOps[A](private val value: NonEmptyLazyList[A])
    extends AnyVal
    with NonEmptyCollection[A, LazyList, NonEmptyLazyList] {

  /**
   * Converts this NonEmptyLazyList to a `LazyList`
   */
  final def toLazyList: LazyList[A] = NonEmptyLazyList.unwrap(value)

  final def map[B](f: A => B): NonEmptyLazyList[B] = create(toLazyList.map(f))

  /**
   * Returns the last element
   */
  final def last: A = toLazyList.last

  /**
   * Returns all elements but the last
   */
  final def init: LazyList[A] = toLazyList.init

  /**
   * Returns the size of this NonEmptyLazyList
   */
  final def size: Int = toLazyList.size

  /**
   * Returns the length of this NonEmptyLazyList
   */
  final def length: Int = toLazyList.length

  /**
   * Returns a new NonEmptyLazyList consisting of `a` followed by this
   */
  final def prepend[AA >: A](a: AA): NonEmptyLazyList[AA] =
    create(a #:: toLazyList)

  /**
   * Alias for [[prepend]].
   */
  final def +:[AA >: A](a: AA): NonEmptyLazyList[AA] =
    prepend(a)

  /**
   * Alias for [[prepend]].
   */
  final def #::[AA >: A](a: AA): NonEmptyLazyList[AA] =
    prepend(a)

  /**
   * Returns a new NonEmptyLazyList consisting of this followed by `a`
   */
  final def append[AA >: A](a: AA): NonEmptyLazyList[AA] =
    create(toLazyList :+ a)

  /**
   * Alias for [[append]].
   */
  final def :+[AA >: A](a: AA): NonEmptyLazyList[AA] =
    append(a)

  /**
   * concatenates this with `ll`
   */
  final def concat[AA >: A](ll: LazyList[AA]): NonEmptyLazyList[AA] =
    create(toLazyList ++ ll)

  /**
   * Concatenates this with `nell`
   */
  final def concatNell[AA >: A](nell: NonEmptyLazyList[AA]): NonEmptyLazyList[AA] =
    create(toLazyList ++ nell.toLazyList)

  /**
   * Alias for concatNell
   */
  final def ++[AA >: A](nell: NonEmptyLazyList[AA]): NonEmptyLazyList[AA] =
    concatNell(nell)

  /**
   * Appends the given LazyList
   */
  final def appendLazyList[AA >: A](nell: LazyList[AA]): NonEmptyLazyList[AA] =
    if (nell.isEmpty) value
    else create(toLazyList ++ nell)

  /**
   * Alias for `appendLazyList`
   */
  final def :++[AA >: A](c: LazyList[AA]): NonEmptyLazyList[AA] =
    appendLazyList(c)

  /**
   * Prepends the given LazyList
   */
  final def prependLazyList[AA >: A](c: LazyList[AA]): NonEmptyLazyList[AA] =
    if (c.isEmpty) value
    else create(c ++ toLazyList)

  /**
   * Prepends the given NonEmptyLazyList
   */
  final def prependNell[AA >: A](c: NonEmptyLazyList[AA]): NonEmptyLazyList[AA] =
    create(c.toLazyList ++ toLazyList)

  /**
   * Alias for `prependNell`
   */
  final def ++:[AA >: A](c: NonEmptyLazyList[AA]): NonEmptyLazyList[AA] =
    prependNell(c)

  /**
   * Converts this NonEmptyLazyList to a `NonEmptyList`.
   */ // TODO also add toNonEmptyLazyList to NonEmptyList?
  final def toNonEmptyList: NonEmptyList[A] =
    NonEmptyList.fromListUnsafe(toLazyList.toList)

  /**
   * Converts this LazyList to a `NonEmptyVector`.
   */
  final def toNonEmptyVector: NonEmptyVector[A] =
    NonEmptyVector.fromVectorUnsafe(toLazyList.toVector)

  /**
   * Returns the first element
   */
  final def head: A = toLazyList.head

  /**
   * Returns all but the first element
   */
  final def tail: LazyList[A] = toLazyList.tail

  /**
   * Tests if some element is contained in this NonEmptyLazyList
   */
  final def contains(a: A)(implicit A: Eq[A]): Boolean =
    toLazyList.contains(a)

  /**
   * Tests whether a predicate holds for all elements
   */
  final def forall(p: A => Boolean): Boolean =
    toLazyList.forall(p)

  /**
   * Tests whether a predicate holds for at least one element of this LazyList
   */
  final def exists(f: A => Boolean): Boolean =
    toLazyList.exists(f)

  /**
   * Returns the first value that matches the given predicate.
   */
  final def find(f: A => Boolean): Option[A] =
    toLazyList.find(f)

  /**
   * Returns a new `LazyList` containing all elements where the result of `pf` is final defined.
   */
  final def collect[B](pf: PartialFunction[A, B]): LazyList[B] =
    toLazyList.collect(pf)

  /**
   * Finds the first element of this `NonEmptyLazyList` for which the given partial
   * function is defined, and applies the partial function to it.
   */
  @deprecated("Use collectFirst", "2.2.0-M1")
  final def collectLazyList[B](pf: PartialFunction[A, B]): Option[B] = collectFirst(pf)

  /**
   * Finds the first element of this `NonEmptyLazyList` for which the given partial
   * function is defined, and applies the partial function to it.
   */
  final def collectFirst[B](pf: PartialFunction[A, B]): Option[B] = toLazyList.collectFirst(pf)

  /**
   * Filters all elements of this NonEmptyLazyList that do not satisfy the given predicate.
   */
  final def filter(p: A => Boolean): LazyList[A] = toLazyList.filter(p)

  /**
   * Filters all elements of this NonEmptyLazyList that satisfy the given predicate.
   */
  final def filterNot(p: A => Boolean): LazyList[A] = filter(t => !p(t))

  /**
   * Left-associative fold using f.
   */
  final def foldLeft[B](b: B)(f: (B, A) => B): B =
    toLazyList.foldLeft(b)(f)

  /**
   * Right-associative fold using f.
   */
  final def foldRight[B](z: B)(f: (A, B) => B): B =
    toLazyList.foldRight(z)(f)

  /**
   * Left-associative reduce using f.
   */
  final def reduceLeft(f: (A, A) => A): A =
    toLazyList.reduceLeft(f)

  /**
   * Apply `f` to the "initial element" of this LazyList and lazily combine it
   * with every other value using the given function `g`.
   */
  final def reduceLeftTo[B](f: A => B)(g: (B, A) => B): B = {
    val iter = toLazyList.iterator
    var result = f(iter.next())
    while (iter.hasNext) { result = g(result, iter.next()) }
    result
  }

  /**
   * Right-associative reduce using f.
   */
  final def reduceRight[AA >: A](f: (A, AA) => AA): AA =
    toLazyList.reduceRight(f)

  /**
   * Apply `f` to the "initial element" of this NonEmptyLazyList and
   * lazily combine it with every other value using the given function `g`.
   */
  final def reduceRightTo[B](f: A => B)(g: (A, B) => B): B = {
    val iter = toLazyList.reverseIterator
    var result = f(iter.next())
    while (iter.hasNext) { result = g(iter.next(), result) }
    result
  }

  /**
   * Reduce using the Semigroup of A
   */
  final def reduce[AA >: A](implicit S: Semigroup[AA]): AA =
    S.combineAllOption(iterator).get

  /**
   * Applies the supplied function to each element and returns a new NonEmptyLazyList from the concatenated results
   */
  final def flatMap[B](f: A => NonEmptyLazyList[B]): NonEmptyLazyList[B] =
    create(toLazyList.flatMap(f.andThen(_.toLazyList)))

  /**
   * Zips this `NonEmptyLazyList` with another `NonEmptyLazyList` and applies a function for each pair of elements
   */
  final def zipWith[B, C](b: NonEmptyLazyList[B])(f: (A, B) => C): NonEmptyLazyList[C] =
    create(toLazyList.zip(b.toLazyList).map { case (a, b) => f(a, b) })

  /**
   * Zips each element of this `NonEmptyLazyList` with its index
   */
  final def zipWithIndex: NonEmptyLazyList[(A, Int)] =
    create(toLazyList.zipWithIndex)

  final def iterator: Iterator[A] = toLazyList.iterator

  final def reverseIterator: Iterator[A] = toLazyList.reverseIterator

  /**
   * Reverses this `NonEmptyLazyList`
   */
  final def reverse: NonEmptyLazyList[A] =
    create(toLazyList.reverse)

  /**
   * Remove duplicates. Duplicates are checked using `Order[_]` instance.
   */
  def distinct[AA >: A](implicit O: Order[AA]): NonEmptyLazyList[AA] = {
    implicit val ord: Ordering[AA] = O.toOrdering

    val buf = LazyList.newBuilder[AA]
    toLazyList.foldLeft(TreeSet.empty[AA]) { (elementsSoFar, a) =>
      if (elementsSoFar(a)) elementsSoFar
      else {
        buf += a; elementsSoFar + a
      }
    }

    create(buf.result())
  }

  /**
   * Sorts this `NonEmptyLazyList` according to an `Order` on transformed `B` from `A`
   *
   * {{{
   * scala> import cats.data.NonEmptyLazyList
   * scala> import cats.instances.int._
   * scala> val nel = NonEmptyLazyList(('a', 4), ('z', 1), ('e', 22))
   * scala> nel.sortBy(_._2).toLazyList.toList
   * res0: List[(Char, Int)] = List((z,1), (a,4), (e,22))
   * }}}
   */
  final def sortBy[B](f: A => B)(implicit B: Order[B]): NonEmptyLazyList[A] =
    // safe: sorting a NonEmptyList cannot produce an empty List
    create(toLazyList.sortBy(f)(B.toOrdering))

  /**
   * Sorts this `NonEmptyList` according to an `Order`
   *
   * {{{
   * scala> import cats.data.NonEmptyLazyList
   * scala> import cats.instances.int._
   * scala> val nel = NonEmptyLazyList(12, 4, 3, 9)
   * scala> nel.sorted.toLazyList.toList
   * res0: List[Int] = List(3, 4, 9, 12)
   * }}}
   */
  final def sorted[AA >: A](implicit AA: Order[AA]): NonEmptyLazyList[AA] =
    create(toLazyList.sorted(AA.toOrdering))

  /**
   * Groups elements inside this `NonEmptyLazyList` according to the `Order`
   * of the keys produced by the given mapping function.
   *
   * {{{
   * scala> import scala.collection.immutable.SortedMap
   * scala> import cats.data.NonEmptyLazyList
   * scala> import cats.implicits._
   * scala> val nel = NonEmptyLazyList(12, -2, 3, -5)
   * scala> val expectedResult = SortedMap(false -> NonEmptyLazyList(-2, -5), true -> NonEmptyLazyList(12, 3))
   * scala> val result = nel.groupBy(_ >= 0)
   * scala> result === expectedResult
   * res0: Boolean = true
   * }}}
   */
  final def groupBy[B](f: A => B)(implicit B: Order[B]): SortedMap[B, NonEmptyLazyList[A]] = {
    implicit val ordering: Ordering[B] = B.toOrdering
    var m = TreeMap.empty[B, mutable.Builder[A, LazyList[A]]]

    for { elem <- toLazyList } {
      val k = f(elem)

      m.get(k) match {
        case None          => m += ((k, LazyList.newBuilder[A] += elem))
        case Some(builder) => builder += elem
      }
    }

    m.map {
      case (k, v) => (k, create(v.result()))
    }: TreeMap[B, NonEmptyLazyList[A]]
  }

  /**
   * Groups elements inside this `NonEmptyLazyList` according to the `Order`
   * of the keys produced by the given mapping function.
   *
   * {{{
   * scala> import cats.data.{NonEmptyLazyList, NonEmptyMap}
   * scala> import cats.implicits._
   * scala> val nel = NonEmptyLazyList(12, -2, 3, -5)
   * scala> val expectedResult = NonEmptyMap.of(false -> NonEmptyLazyList(-2, -5), true -> NonEmptyLazyList(12, 3))
   * scala> val result = nel.groupByNem(_ >= 0)
   * scala> result === expectedResult
   * res0: Boolean = true
   * }}}
   */
  final def groupByNem[B](f: A => B)(implicit B: Order[B]): NonEmptyMap[B, NonEmptyLazyList[A]] =
    NonEmptyMap.fromMapUnsafe(groupBy(f))

  /**
   * Creates new `NonEmptyMap`, similarly to List#toMap from scala standard library.
   * {{{
   * scala> import cats.data.{NonEmptyLazyList, NonEmptyMap}
   * scala> import cats.implicits._
   * scala> val nel = NonEmptyLazyList.fromLazyListPrepend((0, "a"), LazyList((1, "b"),(0, "c"), (2, "d")))
   * scala> val expectedResult = NonEmptyMap.of(0 -> "c", 1 -> "b", 2 -> "d")
   * scala> val result = nel.toNem
   * scala> result === expectedResult
   * res0: Boolean = true
   * }}}
   */
  final def toNem[T, U](implicit ev: A <:< (T, U), order: Order[T]): NonEmptyMap[T, U] =
    NonEmptyMap.fromMapUnsafe(SortedMap(toLazyList.map(ev): _*)(order.toOrdering))

  /**
   * Creates new `NonEmptySet`, similarly to List#toSet from scala standard library.
   * {{{
   * scala> import cats.data._
   * scala> import cats.instances.int._
   * scala> val nel = NonEmptyLazyList.fromLazyListPrepend(1, LazyList(2,2,3,4))
   * scala> nel.toNes
   * res0: cats.data.NonEmptySet[Int] = TreeSet(1, 2, 3, 4)
   * }}}
   */
  final def toNes[B >: A](implicit order: Order[B]): NonEmptySet[B] =
    NonEmptySet.of(head, tail: _*)

  /**
   * Creates new `NonEmptyVector`, similarly to List#toVector from scala standard library.
   * {{{
   * scala> import cats.data._
   * scala> import cats.instances.int._
   * scala> val nel = NonEmptyLazyList.fromLazyListPrepend(1, LazyList(2,3,4))
   * scala> val expectedResult = NonEmptyVector.fromVectorUnsafe(Vector(1,2,3,4))
   * scala> val result = nel.toNev
   * scala> result === expectedResult
   * res0: Boolean = true
   * }}}
   */
  final def toNev[B >: A]: NonEmptyVector[B] =
    NonEmptyVector.fromVectorUnsafe(toLazyList.toVector)

  final def show[AA >: A](implicit AA: Show[AA]): String = s"NonEmpty${Show[LazyList[AA]].show(toLazyList)}"
}

sealed abstract private[data] class NonEmptyLazyListInstances extends NonEmptyLazyListInstances1 {

  implicit val catsDataInstancesForNonEmptyLazyList: Bimonad[NonEmptyLazyList]
    with NonEmptyTraverse[NonEmptyLazyList]
    with SemigroupK[NonEmptyLazyList]
    with Align[NonEmptyLazyList] =
    new AbstractNonEmptyInstances[LazyList, NonEmptyLazyList] with Align[NonEmptyLazyList] {

      def extract[A](fa: NonEmptyLazyList[A]): A = fa.head

      def nonEmptyTraverse[G[_]: Apply, A, B](fa: NonEmptyLazyList[A])(f: A => G[B]): G[NonEmptyLazyList[B]] = {
        def loop(head: A, tail: LazyList[A]): Eval[G[NonEmptyLazyList[B]]] =
          tail.headOption.fold(Eval.now(Apply[G].map(f(head))(NonEmptyLazyList(_)))) { h =>
            Apply[G].map2Eval(f(head), Eval.defer(loop(h, tail.tail)))((b, acc) => b +: acc)
          }

        loop(fa.head, fa.tail).value
      }

      def reduceLeftTo[A, B](fa: NonEmptyLazyList[A])(f: A => B)(g: (B, A) => B): B = fa.reduceLeftTo(f)(g)

      def reduceRightTo[A, B](fa: NonEmptyLazyList[A])(f: A => B)(g: (A, cats.Eval[B]) => cats.Eval[B]): cats.Eval[B] =
        Eval.defer(fa.reduceRightTo(a => Eval.now(f(a))) { (a, b) =>
          Eval.defer(g(a, b))
        })

      private val alignInstance = Align[LazyList].asInstanceOf[Align[NonEmptyLazyList]]

      def functor: Functor[NonEmptyLazyList] = alignInstance.functor

      def align[A, B](fa: NonEmptyLazyList[A], fb: NonEmptyLazyList[B]): NonEmptyLazyList[Ior[A, B]] =
        alignInstance.align(fa, fb)

      override def alignWith[A, B, C](fa: NonEmptyLazyList[A], fb: NonEmptyLazyList[B])(
        f: Ior[A, B] => C
      ): NonEmptyLazyList[C] =
        alignInstance.alignWith(fa, fb)(f)
    }

  implicit def catsDataOrderForNonEmptyLazyList[A: Order]: Order[NonEmptyLazyList[A]] =
    Order[LazyList[A]].asInstanceOf[Order[NonEmptyLazyList[A]]]

  implicit def catsDataSemigroupForNonEmptyLazyList[A]: Semigroup[NonEmptyLazyList[A]] =
    Semigroup[LazyList[A]].asInstanceOf[Semigroup[NonEmptyLazyList[A]]]

  implicit def catsDataShowForNonEmptyLazyList[A](implicit A: Show[A]): Show[NonEmptyLazyList[A]] =
    Show.show[NonEmptyLazyList[A]](_.show)

  implicit def catsDataParallelForNonEmptyLazyList: Parallel.Aux[NonEmptyLazyList, OneAnd[ZipLazyList, *]] =
    new Parallel[NonEmptyLazyList] {
      type F[x] = OneAnd[ZipLazyList, x]

      def applicative: Applicative[OneAnd[ZipLazyList, *]] =
        OneAnd.catsDataApplicativeForOneAnd(ZipLazyList.catsDataAlternativeForZipLazyList)
      def monad: Monad[NonEmptyLazyList] = NonEmptyLazyList.catsDataInstancesForNonEmptyLazyList

      def sequential: OneAnd[ZipLazyList, *] ~> NonEmptyLazyList =
        new (OneAnd[ZipLazyList, *] ~> NonEmptyLazyList) {
          def apply[A](znell: OneAnd[ZipLazyList, A]): NonEmptyLazyList[A] =
            NonEmptyLazyList.fromLazyListPrepend(znell.head, znell.tail.value)
        }

      def parallel: NonEmptyLazyList ~> OneAnd[ZipLazyList, *] =
        new (NonEmptyLazyList ~> OneAnd[ZipLazyList, *]) {
          def apply[A](nell: NonEmptyLazyList[A]): OneAnd[ZipLazyList, A] = OneAnd(nell.head, ZipLazyList(nell.tail))
        }
    }
}

sealed abstract private[data] class NonEmptyLazyListInstances1 extends NonEmptyLazyListInstances2 {

  implicit def catsDataHashForNonEmptyLazyList[A: Hash]: Hash[NonEmptyLazyList[A]] =
    Hash[LazyList[A]].asInstanceOf[Hash[NonEmptyLazyList[A]]]

}

sealed abstract private[data] class NonEmptyLazyListInstances2 extends NonEmptyLazyListInstances3 {
  implicit def catsDataPartialOrderForNonEmptyLazyList[A: PartialOrder]: PartialOrder[NonEmptyLazyList[A]] =
    PartialOrder[LazyList[A]].asInstanceOf[PartialOrder[NonEmptyLazyList[A]]]
}

sealed abstract private[data] class NonEmptyLazyListInstances3 {
  implicit def catsDataEqForNonEmptyLazyList[A: Eq]: Eq[NonEmptyLazyList[A]] =
    Eq[LazyList[A]].asInstanceOf[Eq[NonEmptyLazyList[A]]]
}
