package cats
package data

import cats.instances.list._
import cats.syntax.order._

import scala.annotation.tailrec
import scala.collection.immutable.TreeSet
import scala.collection.mutable.ListBuffer
import scala.collection.{immutable, mutable}

/**
 * A data type which represents a non empty list of A, with
 * single element (head) and optional structure (tail).
 */
final case class NonEmptyList[+A](head: A, tail: List[A]) {

  /**
   * Return the head and tail into a single list
   * {{{
   * scala> import cats.data.NonEmptyList
   * scala> val nel = NonEmptyList.of(1, 2, 3, 4, 5)
   * scala> nel.toList
   * res0: scala.collection.immutable.List[Int] = List(1, 2, 3, 4, 5)
   * }}}
   */
  def toList: List[A] = head :: tail

  /**
   * Selects the last element
   * {{{
   * scala> import cats.data.NonEmptyList
   * scala> val nel = NonEmptyList.of(1, 2, 3, 4, 5)
   * scala> nel.last
   * res0: Int = 5
   * }}}
   */
  def last: A = tail.lastOption match {
    case None    => head
    case Some(a) => a
  }

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
  def init: List[A] = tail match {
    case Nil => List.empty
    case t => head :: t.init
  }

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
  def size: Int = 1 + tail.size

  /**
   *  Applies f to all the elements of the structure
   */
  def map[B](f: A => B): NonEmptyList[B] =
    NonEmptyList(f(head), tail.map(f))

  def ++[AA >: A](l: List[AA]): NonEmptyList[AA] =
    NonEmptyList(head, tail ++ l)

  def flatMap[B](f: A => NonEmptyList[B]): NonEmptyList[B] =
    f(head) ++ tail.flatMap(f andThen (_.toList))

  def ::[AA >: A](a: AA): NonEmptyList[AA] =
    NonEmptyList(a, head :: tail)

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
  def filter(p: A => Boolean): List[A] = {
    val ftail = tail.filter(p)
    if (p(head)) head :: ftail
    else ftail
  }

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
  def filterNot(p: A => Boolean): List[A] = {
    val ftail = tail.filterNot(p)
    if (p(head)) ftail
    else head :: ftail
  }

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
  def collect[B](pf: PartialFunction[A, B]) : List[B] = {
    if (pf.isDefinedAt(head)) {
      pf.apply(head) :: tail.collect(pf)
    } else {
      tail.collect(pf)
    }
  }

  /**
   * Append another NonEmptyList
   */
  def concat[AA >: A](other: NonEmptyList[AA]): NonEmptyList[AA] =
    NonEmptyList(head, tail ::: other.toList)

  /**
   * Find the first element matching the predicate, if one exists
   */
  def find(p: A => Boolean): Option[A] =
    if (p(head)) Some(head)
    else tail.find(p)

  /**
   * Check whether at least one element satisfies the predicate
   */
  def exists(p: A => Boolean): Boolean =
    p(head) || tail.exists(p)

  /**
   * Check whether all elements satisfy the predicate
   */
  def forall(p: A => Boolean): Boolean =
    p(head) && tail.forall(p)

  /**
   * Left-associative fold on the structure using f.
   */
  def foldLeft[B](b: B)(f: (B, A) => B): B =
    tail.foldLeft(f(b, head))(f)

  /**
   * Right-associative fold on the structure using f.
   */
  def foldRight[B](lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    Foldable[List].foldRight(toList, lb)(f)

  /**
   * Left-associative reduce using f.
   */
  def reduceLeft[AA >: A](f: (AA, AA) => AA): AA =
    tail.foldLeft[AA](head)(f)

  /**
   * Reduce using the `Semigroup` of `AA`.
   */
  def reduce[AA >: A](implicit S: Semigroup[AA]): AA =
    S.combineAllOption(toList).get

  def traverse[G[_], B](f: A => G[B])(implicit G: Applicative[G]): G[NonEmptyList[B]] =
    G.map2Eval(f(head), Always(Traverse[List].traverse(tail)(f)))(NonEmptyList(_, _)).value

  def coflatMap[B](f: NonEmptyList[A] => B): NonEmptyList[B] = {
    val buf = ListBuffer.empty[B]
    @tailrec def consume(as: List[A]): List[B] =
      as match {
        case Nil => buf.toList
        case a :: as =>
          buf += f(NonEmptyList(a, as))
          consume(as)
      }
    NonEmptyList(f(this), consume(tail))
  }

  def ===[AA >: A](o: NonEmptyList[AA])(implicit AA: Eq[AA]): Boolean =
    ((this.head: AA) === o.head) && (this.tail: List[AA]) === o.tail

  def show[AA >: A](implicit AA: Show[AA]): String =
    toList.iterator.map(AA.show).mkString("NonEmptyList(", ", ", ")")

  override def toString: String = s"NonEmpty$toList"

  /**
   * Remove duplicates. Duplicates are checked using `Order[_]` instance.
   */
  def distinct[AA >: A](implicit O: Order[AA]): NonEmptyList[AA] = {
    implicit val ord = O.toOrdering

    val buf = ListBuffer.empty[AA]
    tail.foldLeft(TreeSet(head: AA)) { (elementsSoFar, b) =>
      if (elementsSoFar(b)) elementsSoFar else { buf += b; elementsSoFar + b }
    }

    NonEmptyList(head, buf.toList)
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
  def reverse: NonEmptyList[A] = {
    @tailrec
    def go(h: A, rest: List[A], acc: List[A]): NonEmptyList[A] =
      rest match {
        case Nil => NonEmptyList(h, acc)
        case h1 :: t1 => go(h1, t1, h :: acc)
      }
    go(head, tail, Nil)
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
  def zipWithIndex: NonEmptyList[(A, Int)] = {
    val bldr = List.newBuilder[(A, Int)]
    var idx = 1
    val it = tail.iterator
    while (it.hasNext) {
      bldr += ((it.next, idx))
      idx += 1
    }
    NonEmptyList((head, 0), bldr.result)
  }

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
    toList.sortBy(f)(B.toOrdering) match {
      case x :: xs => NonEmptyList(x, xs)
      case Nil     => sys.error("unreachable: sorting a NonEmptyList cannot produce an empty List")
    }

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
    toList.sorted(AA.toOrdering) match {
      case x :: xs => NonEmptyList(x, xs)
      case Nil     => sys.error("unreachable: sorting a NonEmptyList cannot produce an empty List")
    }

  /**
    * Groups elements inside of this `NonEmptyList` using a mapping function
    *
    * {{{
    * scala> import cats.data.NonEmptyList
    * scala> val nel = NonEmptyList.of(12, -2, 3, -5)
    * scala> nel.groupBy(_ >= 0)
    * res0: Map[Boolean, cats.data.NonEmptyList[Int]] = Map(false -> NonEmptyList(-2, -5), true -> NonEmptyList(12, 3))
    * }}}
    */
  def groupBy[B](f: A => B): Map[B, NonEmptyList[A]] = {
    val m = mutable.Map.empty[B, mutable.Builder[A, List[A]]]
    for { elem <- toList } {
      m.getOrElseUpdate(f(elem), List.newBuilder[A]) += elem
    }
    val b = immutable.Map.newBuilder[B, NonEmptyList[A]]
    for { (k, v) <- m } {
      val head :: tail = v.result // we only create non empty list inside of the map `m`
      b += ((k, NonEmptyList(head, tail)))
    }
    b.result
  }

}

object NonEmptyList extends NonEmptyListInstances {
  def of[A](head: A, tail: A*): NonEmptyList[A] = NonEmptyList(head, tail.toList)

  def one[A](head: A): NonEmptyList[A] = NonEmptyList(head, Nil)

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
      case Nil => None
      case h :: t => Some(NonEmptyList(h, t))
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
      case Nil => throw new IllegalArgumentException("Cannot create NonEmptyList from empty list")
      case h :: t => NonEmptyList(h, t)
    }

  def fromFoldable[F[_], A](fa: F[A])(implicit F: Foldable[F]): Option[NonEmptyList[A]] =
    fromList(F.toList(fa))

  def fromReducible[F[_], A](fa: F[A])(implicit F: Reducible[F]): NonEmptyList[A] =
    F.toNonEmptyList(fa)
}

private[data] sealed trait NonEmptyListInstances extends NonEmptyListInstances0 {

  implicit val catsDataInstancesForNonEmptyList: SemigroupK[NonEmptyList] with Reducible[NonEmptyList]
      with Comonad[NonEmptyList] with NonEmptyTraverse[NonEmptyList] with Monad[NonEmptyList] =
    new NonEmptyReducible[NonEmptyList, List] with SemigroupK[NonEmptyList] with Comonad[NonEmptyList]
      with Monad[NonEmptyList] with NonEmptyTraverse[NonEmptyList] {

      def combineK[A](a: NonEmptyList[A], b: NonEmptyList[A]): NonEmptyList[A] =
        a concat b

      override def split[A](fa: NonEmptyList[A]): (A, List[A]) = (fa.head, fa.tail)

      override def reduceLeft[A](fa: NonEmptyList[A])(f: (A, A) => A): A =
        fa.reduceLeft(f)

      override def reduce[A](fa: NonEmptyList[A])(implicit A: Semigroup[A]): A =
        fa.reduce

      override def map[A, B](fa: NonEmptyList[A])(f: A => B): NonEmptyList[B] =
        fa map f

      def pure[A](x: A): NonEmptyList[A] =
        NonEmptyList.one(x)

      def flatMap[A, B](fa: NonEmptyList[A])(f: A => NonEmptyList[B]): NonEmptyList[B] =
        fa flatMap f

      def coflatMap[A, B](fa: NonEmptyList[A])(f: NonEmptyList[A] => B): NonEmptyList[B] =
        fa coflatMap f

      def extract[A](fa: NonEmptyList[A]): A = fa.head

      def nonEmptyTraverse[G[_], A, B](nel: NonEmptyList[A])(f: A => G[B])(implicit G: Apply[G]): G[NonEmptyList[B]] =
        Foldable[List].reduceRightToOption[A, G[List[B]]](nel.tail)(a => G.map(f(a))(_ :: Nil)) { (a, lglb) =>
          G.map2Eval(f(a), lglb)(_ :: _)
        }.map {
          case None => G.map(f(nel.head))(NonEmptyList(_, Nil))
          case Some(gtail) => G.map2(f(nel.head), gtail)(NonEmptyList(_, _))
        }.value

      override def traverse[G[_], A, B](fa: NonEmptyList[A])(f: A => G[B])(implicit G: Applicative[G]): G[NonEmptyList[B]] =
        fa traverse f

      override def foldLeft[A, B](fa: NonEmptyList[A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b)(f)

      override def foldRight[A, B](fa: NonEmptyList[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        fa.foldRight(lb)(f)

      def tailRecM[A, B](a: A)(f: A => NonEmptyList[Either[A, B]]): NonEmptyList[B] = {
        val buf = new ListBuffer[B]
        @tailrec def go(v: NonEmptyList[Either[A, B]]): Unit = v.head match {
            case Right(b) =>
            buf += b
            NonEmptyList.fromList(v.tail) match {
              case Some(t) => go(t)
              case None => ()
            }
          case Left(a) => go(f(a) ++ v.tail)
          }
        go(f(a))
        NonEmptyList.fromListUnsafe(buf.result())
      }

      override def fold[A](fa: NonEmptyList[A])(implicit A: Monoid[A]): A =
        fa.reduce

      override def find[A](fa: NonEmptyList[A])(f: A => Boolean): Option[A] =
        fa find f

      override def forall[A](fa: NonEmptyList[A])(p: A => Boolean): Boolean =
        fa forall p

      override def exists[A](fa: NonEmptyList[A])(p: A => Boolean): Boolean =
        fa exists p

      override def toList[A](fa: NonEmptyList[A]): List[A] = fa.toList

      override def toNonEmptyList[A](fa: NonEmptyList[A]): NonEmptyList[A] = fa

      override def get[A](fa: NonEmptyList[A])(idx: Long): Option[A] =
        if (idx == 0) Some(fa.head) else Foldable[List].get(fa.tail)(idx - 1)
    }

  implicit def catsDataShowForNonEmptyList[A](implicit A: Show[A]): Show[NonEmptyList[A]] =
    Show.show[NonEmptyList[A]](_.show)

  implicit def catsDataSemigroupForNonEmptyList[A]: Semigroup[NonEmptyList[A]] =
    SemigroupK[NonEmptyList].algebra[A]

  implicit def catsDataOrderForNonEmptyList[A](implicit A: Order[A]): Order[NonEmptyList[A]] =
    new NonEmptyListOrder[A] {
      val A0 = A
    }
}

private[data] sealed trait NonEmptyListInstances0 extends NonEmptyListInstances1 {
  implicit def catsDataPartialOrderForNonEmptyList[A](implicit A: PartialOrder[A]): PartialOrder[NonEmptyList[A]] =
    new NonEmptyListPartialOrder[A] {
      val A0 = A
    }
}

private[data] sealed trait NonEmptyListInstances1 {

  implicit def catsDataEqForNonEmptyList[A](implicit A: Eq[A]): Eq[NonEmptyList[A]] =
    new NonEmptyListEq[A] {
      val A0 = A
    }
}

private[data] sealed trait NonEmptyListEq[A] extends Eq[NonEmptyList[A]] {
  implicit def A0: Eq[A]

  override def eqv(x: NonEmptyList[A], y: NonEmptyList[A]): Boolean = x === y
}

private[data] sealed trait NonEmptyListPartialOrder[A] extends PartialOrder[NonEmptyList[A]] with NonEmptyListEq[A] {
  override implicit def A0: PartialOrder[A]

  override def partialCompare(x: NonEmptyList[A], y: NonEmptyList[A]): Double =
    x.toList partialCompare y.toList
}

private[data] sealed abstract class NonEmptyListOrder[A] extends Order[NonEmptyList[A]] with NonEmptyListPartialOrder[A] {
  override implicit def A0: Order[A]

  override def compare(x: NonEmptyList[A], y: NonEmptyList[A]): Int =
    x.toList compare y.toList
}
