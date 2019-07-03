package cats
package data

import cats.data.NonEmptyList.ZipNonEmptyList
import cats.instances.list._

import scala.collection.immutable.{SortedMap, SortedSet, TreeMap, TreeSet}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import NonEmptyList.create

import scala.annotation.tailrec
/**
 * A data type which represents a non empty list of A, with
 * single element (head) and optional structure (tail).
 */
object NonEmptyList extends NonEmptyListInstances {
  private[data] type Base
  private[data] trait Tag extends Any
  type Type[+A] <: Base with Tag

  private[cats] def create[A](s: List[A]): Type[A] =
    s.asInstanceOf[Type[A]]

  private[cats] def unwrap[A](s: Type[A]): List[A] =
    s.asInstanceOf[List[A]]

  def of[A](head: A, tail: A*): NonEmptyList[A] = apply(head, tail.toList)

  def apply[A](head: A, tail: List[A]): NonEmptyList[A] = create(head :: tail)

  def ofInitLast[A](init: List[A], last: A): NonEmptyList[A] =
    create(init :+ last)

  def one[A](head: A): NonEmptyList[A] = create(List(head))

  def unapply[A](nel: NonEmptyList[A]): Option[(A, List[A])] =
    Some((nel.head, nel.tail))

  /**
   * Create a `NonEmptyList` from a `List`.
   *
   * The result will be `None` if the input list is empty and `Some` wrapping a
   * `NonEmptyList` otherwise.
   *
   * @see [[fromListUnsafe]] for an unsafe version that throws an exception if
   * the input list is empty.
   */
  def fromList[A](l: List[A]): Option[NonEmptyList[A]] =
    l match {
      case Nil    => None
      case _ => Some(create(l))
    }

  /**
   * Create a `NonEmptyList` from a `List`, or throw an
   * `IllegalArgumentException` if the input list is empty.
   *
   * @see [[fromList]] for a safe version that returns `None` if the input list
   * is empty.
   */
  def fromListUnsafe[A](l: List[A]): NonEmptyList[A] =
    l match {
      case Nil    => throw new IllegalArgumentException("Cannot create NonEmptyList from empty list")
      case _ => create(l)
    }

  def fromFoldable[F[_], A](fa: F[A])(implicit F: Foldable[F]): Option[NonEmptyList[A]] =
    fromList(F.toList(fa))

  def fromReducible[F[_], A](fa: F[A])(implicit F: Reducible[F]): NonEmptyList[A] =
    F.toNonEmptyList(fa)

  class ZipNonEmptyList[A](val value: NonEmptyList[A]) extends Serializable

  object ZipNonEmptyList {

    def apply[A](nev: NonEmptyList[A]): ZipNonEmptyList[A] =
      new ZipNonEmptyList(nev)

    implicit val catsDataCommutativeApplyForZipNonEmptyList: CommutativeApply[ZipNonEmptyList] =
      new CommutativeApply[ZipNonEmptyList] {
        def ap[A, B](ff: ZipNonEmptyList[A => B])(fa: ZipNonEmptyList[A]): ZipNonEmptyList[B] =
          ZipNonEmptyList(ff.value.zipWith(fa.value)(_.apply(_)))

        override def map[A, B](fa: ZipNonEmptyList[A])(f: (A) => B): ZipNonEmptyList[B] =
          ZipNonEmptyList(fa.value.map(f))

        override def product[A, B](fa: ZipNonEmptyList[A], fb: ZipNonEmptyList[B]): ZipNonEmptyList[(A, B)] =
          ZipNonEmptyList(fa.value.zipWith(fb.value) { case (a, b) => (a, b) })
      }

    implicit def zipNelEq[A: Eq]: Eq[ZipNonEmptyList[A]] = Eq.by(_.value)
  }

  implicit def catsNonEmptyListOps[A](value: NonEmptyList[A]): NonEmptyListOps[A] =
    new NonEmptyListOps(value)
}

class NonEmptyListOps[A](private val value: NonEmptyList[A]) extends AnyVal {

  /**
   * Return the head and tail into a single list
   * {{{
   * scala> import cats.data.NonEmptyList
   * scala> val nel = NonEmptyList.of(1, 2, 3, 4, 5)
   * scala> nel.toList
   * res0: scala.collection.immutable.List[Int] = List(1, 2, 3, 4, 5)
   * }}}
   */
  def toList: List[A] = NonEmptyList.unwrap(value)

  def head: A = toList.head

  def tail: List[A] = toList.tail

  /**
   * Selects the last element
   * {{{
   * scala> import cats.data.NonEmptyList
   * scala> val nel = NonEmptyList.of(1, 2, 3, 4, 5)
   * scala> nel.last
   * res0: Int = 5
   * }}}
   */
  def last: A = toList.last

  /**
   * Selects all elements except the last
   *
   * {{{
   * scala> import cats.data.NonEmptyList
   * scala> val nel = NonEmptyList.of(1, 2, 3, 4, 5)
   * scala> nel.init
   * res0: scala.collection.immutable.List[Int] = List(1, 2, 3, 4)
   * }}}
   */
  def init: List[A] = toList.init

  /**
   * The size of this NonEmptyList
   *
   * {{{
   * scala> import cats.data.NonEmptyList
   * scala> val nel = NonEmptyList.of(1, 2, 3, 4, 5)
   * scala> nel.size
   * res0: Int = 5
   * }}}
   */
  def size: Int =  toList.size

  def length: Int = size

  /**
   *  Applies f to all the elements of the structure
   */
  def map[B](f: A => B): NonEmptyList[B] = create(toList.map(f))

  def ++[AA >: A](l: List[AA]): NonEmptyList[AA] =
    concat(l)

  def concat[AA >: A](other: List[AA]): NonEmptyList[AA] =
    create(toList ::: other)

  /**
   * Append another NonEmptyList
   */
  def concatNel[AA >: A](other: NonEmptyList[AA]): NonEmptyList[AA] =
    concat(other.toList)

  def flatMap[B](f: A => NonEmptyList[B]): NonEmptyList[B] =
    create(toList.flatMap(f.andThen(_.toList)))


  def ::[AA >: A](a: AA): NonEmptyList[AA] =
    prepend(a)

  def prepend[AA >: A](a: AA): NonEmptyList[AA] =
    create(a :: toList)

  /**
   * Alias for append
   *
   * {{{
   * scala> import cats.data.NonEmptyList
   * scala> val nel = NonEmptyList.of(1, 2, 3)
   * scala> nel :+ 4
   * res0: cats.data.NonEmptyList[Int] = NonEmptyList(1, 2, 3, 4)
   * }}}
   */
  def :+[AA >: A](a: AA): NonEmptyList[AA] =
    append(a)

  def append[AA >: A](a: AA): NonEmptyList[AA] =
    create(toList :+ a)

  /**
   * Alias for concatNel
   *
   * {{{
   * scala> import cats.data.NonEmptyList
   * scala> val nel = NonEmptyList.of(1, 2, 3)
   * scala> nel ::: NonEmptyList.of(4, 5)
   * res0: cats.data.NonEmptyList[Int] = NonEmptyList(1, 2, 3, 4, 5)
   * }}}
   */
  def :::[AA >: A](other: NonEmptyList[AA]): NonEmptyList[AA] =
    other.concatNel(value)

  /**
   * Remove elements not matching the predicate
   *
   * {{{
   * scala> import cats.data.NonEmptyList
   * scala> val nel = NonEmptyList.of(1, 2, 3, 4, 5)
   * scala> nel.filter(_ < 3)
   * res0: scala.collection.immutable.List[Int] = List(1, 2)
   * }}}
   */
  def filter(p: A => Boolean): List[A] =
    toList.filter(p)

  /**
   * Remove elements matching the predicate
   *
   * {{{
   * scala> import cats.data.NonEmptyList
   * scala> val nel = NonEmptyList.of(1, 2, 3, 4, 5)
   * scala> nel.filterNot(_ < 3)
   * res0: scala.collection.immutable.List[Int] = List(3, 4, 5)
   * }}}
   */
  def filterNot(p: A => Boolean): List[A] =
    toList.filterNot(p)

  /**
   * Builds a new `List` by applying a partial function to
   * all the elements from this `NonEmptyList` on which the function is defined
   *
   * {{{
   * scala> import cats.data.NonEmptyList
   * scala> val nel = NonEmptyList.of(1, 2, 3, 4, 5)
   * scala> nel.collect { case v if v < 3 => v }
   * res0: scala.collection.immutable.List[Int] = List(1, 2)
   * scala> nel.collect {
   *      |  case v if v % 2 == 0 => "even"
   *      |  case _ => "odd"
   *      | }
   * res1: scala.collection.immutable.List[String] = List(odd, even, odd, even, odd)
   * }}}
   */
  def collect[B](pf: PartialFunction[A, B]): List[B] =
    toList.collect(pf)

  /**
   * Find the first element matching the predicate, if one exists
   */
  def find(p: A => Boolean): Option[A] =
    toList.find(p)

  /**
   * Check whether at least one element satisfies the predicate
   */
  def exists(p: A => Boolean): Boolean =
    toList.exists(p)

  /**
   * Check whether all elements satisfy the predicate
   */
  def forall(p: A => Boolean): Boolean =
    toList.forall(p)

  /**
   * Left-associative fold on the structure using f.
   */
  def foldLeft[B](b: B)(f: (B, A) => B): B =
    toList.foldLeft(b)(f)

  /**
   * Right-associative fold on the structure using f.
   */
  def foldRight[B](lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    Foldable[List].foldRight(toList, lb)(f)

  /**
   * Left-associative reduce using f.
   */
  def reduceLeft[AA >: A](f: (AA, AA) => AA): AA =
    toList.reduceLeft(f)

  /**
   * Reduce using the `Semigroup` of `AA`.
   */
  def reduce[AA >: A](implicit S: Semigroup[AA]): AA =
    S.combineAllOption(toList).get

  def traverse[G[_], B](f: A => G[B])(implicit G: Applicative[G]): G[NonEmptyList[B]] =
    Traverse[List].traverse(toList)(f).asInstanceOf[G[NonEmptyList[B]]]

  def coflatMap[B](f: NonEmptyList[A] => B): NonEmptyList[B] =
    create(CoflatMap[List].coflatMap(toList)(f.asInstanceOf[List[A] => B]))

  def ===[AA >: A](o: NonEmptyList[AA])(implicit AA: Eq[AA]): Boolean =
    Eq[List[AA]].eqv(toList, o.toList)

  def show[AA >: A](implicit AA: Show[AA]): String =
    s"NonEmpty${Show[List[AA]].show(toList)}"

    /**
   * Remove duplicates. Duplicates are checked using `Order[_]` instance.
   */
  def distinct[AA >: A](implicit O: Order[AA]): NonEmptyList[AA] = {
    implicit val ord = O.toOrdering

    val buf = ListBuffer.empty[AA]
    toList.foldLeft(TreeSet.empty[AA]) { (elementsSoFar, b) =>
      if (elementsSoFar(b)) elementsSoFar
      else {
        buf += b; elementsSoFar + b
      }
    }

    create(buf.toList)
  }


  /**
   * Apply `f` to the "initial element" of this LazyList and lazily combine it
   * with every other value using the given function `g`.
   */
  final def reduceLeftTo[B](f: A => B)(g: (B, A) => B): B = {
    val iter = toList.iterator
    var result = f(iter.next)
    while (iter.hasNext) { result = g(result, iter.next) }
    result
  }


  /**
   * Right-associative reduce using f.
   */
  final def reduceRight[AA >: A](f: (A, AA) => AA): AA =
    toList.reduceRight(f)

  /**
   * Apply `f` to the "initial element" of this NonEmptyLazyList and
   * lazily combine it with every other value using the given function `g`.
   */
  final def reduceRightTo[B](f: A => B)(g: (A, B) => B): B = {
    val iter = toList.reverseIterator
    var result = f(iter.next)
    while (iter.hasNext) { result = g(iter.next, result) }
    result
  }

  /**
   * Reverse this `NonEmptyList`.
   *
   * {{{
   * scala> import cats.data.NonEmptyList
   * scala> val nel = NonEmptyList.of(1, 2, 3)
   * scala> nel.reverse
   * res0: cats.data.NonEmptyList[Int] = NonEmptyList(3, 2, 1)
   * }}}
   */
  def reverse: NonEmptyList[A] =
    create(toList.reverse)

  /**
   * Zips this `NonEmptyList` with another `NonEmptyList` and applies a function for each pair of elements.
   *
   * {{{
   * scala> import cats.data.NonEmptyList
   * scala> val as = NonEmptyList.of(1, 2, 3)
   * scala> val bs = NonEmptyList.of("A", "B", "C")
   * scala> as.zipWith(bs)(_ + _)
   * res0: cats.data.NonEmptyList[String] = NonEmptyList(1A, 2B, 3C)
   * }}}
   */
  def zipWith[B, C](b: NonEmptyList[B])(f: (A, B) => C): NonEmptyList[C] = {
    @tailrec
    def zwRev(as: List[A], bs: List[B], acc: List[C]): List[C] = (as, bs) match {
      case (Nil, _) => acc
      case (_, Nil) => acc
      case (x :: xs, y :: ys) => zwRev(xs, ys, f(x, y) :: acc)
    }

    create(zwRev(toList, b.toList, Nil).reverse)
  }

  /**
   * Zips each element of this `NonEmptyList` with its index.
   *
   * {{{
   * scala> import cats.data.NonEmptyList
   * scala> val nel = NonEmptyList.of("a", "b", "c")
   * scala> nel.zipWithIndex
   * res0: cats.data.NonEmptyList[(String, Int)] = NonEmptyList((a,0), (b,1), (c,2))
   * }}}
   */
  def zipWithIndex: NonEmptyList[(A, Int)] =
    create(toList.zipWithIndex)

  /**
   * Sorts this `NonEmptyList` according to an `Order` on transformed `B` from `A`
   *
   * {{{
   * scala> import cats.data.NonEmptyList
   * scala> import cats.instances.int._
   * scala> val nel = NonEmptyList.of(('a', 4), ('z', 1), ('e', 22))
   * scala> nel.sortBy(_._2)
   * res0: cats.data.NonEmptyList[(Char, Int)] = NonEmptyList((z,1), (a,4), (e,22))
   * }}}
   */
  def sortBy[B](f: A => B)(implicit B: Order[B]): NonEmptyList[A] =
  // safe: sorting a NonEmptyList cannot produce an empty List
    NonEmptyList.fromListUnsafe(toList.sortBy(f)(B.toOrdering))

  /**
   * Sorts this `NonEmptyList` according to an `Order`
   *
   * {{{
   * scala> import cats.data.NonEmptyList
   * scala> import cats.instances.int._
   * scala> val nel = NonEmptyList.of(12, 4, 3, 9)
   * scala> nel.sorted
   * res0: cats.data.NonEmptyList[Int] = NonEmptyList(3, 4, 9, 12)
   * }}}
   */
  def sorted[AA >: A](implicit AA: Order[AA]): NonEmptyList[AA] =
  // safe: sorting a NonEmptyList cannot produce an empty List
    NonEmptyList.fromListUnsafe(toList.sorted(AA.toOrdering))

  /**
   * Groups elements inside this `NonEmptyList` according to the `Order`
   * of the keys produced by the given mapping function.
   *
   * {{{
   * scala> import scala.collection.immutable.SortedMap
   * scala> import cats.data.NonEmptyList
   * scala> import cats.instances.boolean._
   * scala> val nel = NonEmptyList.of(12, -2, 3, -5)
   * scala> nel.groupBy(_ >= 0)
   * res0: SortedMap[Boolean, cats.data.NonEmptyList[Int]] = Map(false -> NonEmptyList(-2, -5), true -> NonEmptyList(12, 3))
   * }}}
   */
  def groupBy[B](f: A => B)(implicit B: Order[B]): SortedMap[B, NonEmptyList[A]] = {
    implicit val ordering: Ordering[B] = B.toOrdering
    var m = TreeMap.empty[B, mutable.Builder[A, List[A]]]

    for { elem <- toList } {
      val k = f(elem)

      m.get(k) match {
        case None          => m += ((k, List.newBuilder[A] += elem))
        case Some(builder) => builder += elem
      }
    }

    m.map {
      case (k, v) => (k, NonEmptyList.fromListUnsafe(v.result))
    }: TreeMap[B, NonEmptyList[A]]
  }

  /**
   * Groups elements inside this `NonEmptyList` according to the `Order`
   * of the keys produced by the given mapping function.
   *
   * {{{
   * scala> import cats.data._
   * scala> import cats.instances.boolean._
   * scala> val nel = NonEmptyList.of(12, -2, 3, -5)
   * scala> nel.groupByNem(_ >= 0)
   * res0: NonEmptyMap[Boolean, NonEmptyList[Int]] = Map(false -> NonEmptyList(-2, -5), true -> NonEmptyList(12, 3))
   * }}}
   */
  def groupByNem[B](f: A => B)(implicit B: Order[B]): NonEmptyMap[B, NonEmptyList[A]] =
    NonEmptyMap.fromMapUnsafe(groupBy(f))

  /**
   * Creates new `NonEmptyMap`, similarly to List#toMap from scala standard library.
   *{{{
   * scala> import cats.data._
   * scala> import cats.instances.int._
   * scala> val nel = NonEmptyList((0, "a"), List((1, "b"),(0, "c"), (2, "d")))
   * scala> nel.toNem
   * res0: NonEmptyMap[Int,String] = Map(0 -> c, 1 -> b, 2 -> d)
   *}}}
   *
   */
  def toNem[T, U](implicit ev: A <:< (T, U), order: Order[T]): NonEmptyMap[T, U] =
    NonEmptyMap.fromMapUnsafe(SortedMap(toList.map(ev): _*)(order.toOrdering))

  /**
   * Creates new `NonEmptySet`, similarly to List#toSet from scala standard library.
   *{{{
   * scala> import cats.data._
   * scala> import cats.instances.int._
   * scala> val nel = NonEmptyList(1, List(2,2,3,4))
   * scala> nel.toNes
   * res0: cats.data.NonEmptySet[Int] = TreeSet(1, 2, 3, 4)
   *}}}
   */
  def toNes[B >: A](implicit order: Order[B]): NonEmptySet[B] =
    NonEmptySet.fromSetUnsafe(SortedSet[B](toList: _*)(order.toOrdering))
}


sealed abstract private[data] class NonEmptyListInstances  extends NonEmptyListInstances1  {

  implicit val catsDataInstancesForNonEmptyList: Bimonad[NonEmptyList] with NonEmptyTraverse[NonEmptyList] =
    new AbstractNonEmptyBimonadTraverse[List, NonEmptyList] {

      def extract[A](fa: NonEmptyList[A]): A = fa.head

      def nonEmptyTraverse[G[_]: Apply, A, B](fa: NonEmptyList[A])(f: A => G[B]): G[NonEmptyList[B]] =
        Foldable[List].reduceRightToOption[A, G[List[B]]](fa.tail)(a => Apply[G].map(f(a))(List.apply(_))) { (a, lglb) =>
          Apply[G].map2Eval(f(a), lglb)(_ +: _)
        }.map {
          case None        => Apply[G].map(f(fa.head))(h => create(List(h)))
          case Some(gtail) => Apply[G].map2(f(fa.head), gtail)((h, t) => create(List(h) ++ t))
        } .value

      def reduceLeftTo[A, B](fa: NonEmptyList[A])(f: A => B)(g: (B, A) => B): B = fa.reduceLeftTo(f)(g)

      def reduceRightTo[A, B](fa: NonEmptyList[A])(f: A => B)(g: (A, cats.Eval[B]) => cats.Eval[B]): cats.Eval[B] =
        Eval.defer(fa.reduceRightTo(a => Eval.now(f(a))) { (a, b) =>
          Eval.defer(g(a, b))
        })

    }

  implicit def catsDataOrderForNonEmptyList[A: Order]: Order[NonEmptyList[A]] =
    Order[List[A]].asInstanceOf[Order[NonEmptyList[A]]]

  implicit def catsDataSemigroupForNonEmptyList[A]: Semigroup[NonEmptyList[A]] =
    Semigroup[List[A]].asInstanceOf[Semigroup[NonEmptyList[A]]]

  implicit def catsDataShowForNonEmptyList[A](implicit A: Show[A]): Show[NonEmptyList[A]] =
    Show.show[NonEmptyList[A]](_.show)

  implicit def catsDataNonEmptyParallelForNonEmptyList[A]: NonEmptyParallel[NonEmptyList, ZipNonEmptyList] =
    new NonEmptyParallel[NonEmptyList, ZipNonEmptyList] {

      def flatMap: FlatMap[NonEmptyList] = NonEmptyList.catsDataInstancesForNonEmptyList

      def apply: Apply[ZipNonEmptyList] = ZipNonEmptyList.catsDataCommutativeApplyForZipNonEmptyList

      def sequential: ZipNonEmptyList ~> NonEmptyList =
        λ[ZipNonEmptyList ~> NonEmptyList](_.value)

      def parallel: NonEmptyList ~> ZipNonEmptyList =
        λ[NonEmptyList ~> ZipNonEmptyList](nel => new ZipNonEmptyList(nel))
    }

}

sealed abstract private[data] class NonEmptyListInstances1  extends NonEmptyListInstances2  {
  implicit val catsDataSemigroupKForNonEmptyList: SemigroupK[NonEmptyList] =
    SemigroupK[List].asInstanceOf[SemigroupK[NonEmptyList]]

  implicit def catsDataHashForNonEmptyList[A: Hash]: Hash[NonEmptyList[A]] =
    Hash[List[A]].asInstanceOf[Hash[NonEmptyList[A]]]

}

sealed abstract private[data] class NonEmptyListInstances2 extends NonEmptyListInstances3 {
  implicit def catsDataPartialOrderForNonEmptyList[A: PartialOrder]: PartialOrder[NonEmptyList[A]] =
    PartialOrder[List[A]].asInstanceOf[PartialOrder[NonEmptyList[A]]]
}

sealed abstract private[data] class NonEmptyListInstances3 {
  implicit def catsDataEqForNonEmptyList[A: Eq]: Eq[NonEmptyList[A]] =
    Eq[List[A]].asInstanceOf[Eq[NonEmptyList[A]]]
}


