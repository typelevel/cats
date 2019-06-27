package cats.data

import cats.data.NonEmptyLazyList.ZipNonEmptyLazyList
import cats.instances.lazyList._
import cats.kernel.compat.scalaVersionSpecific.LazyList
import cats.syntax.order._

import scala.annotation.tailrec
import scala.collection.immutable.{SortedMap, TreeMap, TreeSet}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * A data type which represents a non empty lazy list of A, with
  * single element (head) and optional structure (tail).
  */
// TODO *** considering implementing as non-case class so that head can be call-by-name
final case class NonEmptyLazyList[+A](head: A, tail: LazyList[A]) {

  /**
    * Return the head and tail into a single list
    * {{{
    * scala> import cats.data.NonEmptyLazyList
    * scala> val nell = NonEmptyLazyList.of(1, 2, 3, 4, 5)
    * scala> nell.toLazyList
    * res0: scala.collection.immutable.LazyList[Int] = LazyList(1, 2, 3, 4, 5)
    * }}}
    */
  def toLazyList: LazyList[A] = head #:: tail

  /**
    * Selects the last element
    * {{{
    * scala> import cats.data.NonEmptyLazyList
    * scala> val nell = NonEmptyLazyList.of(1, 2, 3, 4, 5)
    * scala> nell.last
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
    * scala> import cats.data.NonEmptyLazyList
    * scala> val nell = NonEmptyLazyList.of(1, 2, 3, 4, 5)
    * scala> nell.init
    * res0: scala.collection.immutable.LazyList[Int] = LazyList(1, 2, 3, 4)
    * }}}
    */
  def init: LazyList[A] = tail match {
    case Nil => LazyList.empty
    case t   => head #:: t.init
  }

  /**
    * The size of this NonEmptyLazyList
    *
    * {{{
    * scala> import cats.data.NonEmptyLazyList
    * scala> val nell = NonEmptyLazyList.of(1, 2, 3, 4, 5)
    * scala> nell.size
    * res0: Int = 5
    * }}}
    */
  def size: Int = 1 + tail.size

  def length: Int = size

  /**
    *  Applies f to all the elements of the structure
    */
  def map[B](f: A => B): NonEmptyLazyList[B] =
    NonEmptyLazyList(f(head), tail.map(f))

  def ++[AA >: A](l: LazyList[AA]): NonEmptyLazyList[AA] =
    concat(l)

  def concat[AA >: A](other: LazyList[AA]): NonEmptyLazyList[AA] =
    NonEmptyLazyList(head, tail #::: other)

  /**
    * Append another NonEmptyLazyList
    */
  def concatNel[AA >: A](other: NonEmptyLazyList[AA]): NonEmptyLazyList[AA] =
    NonEmptyLazyList(head, tail #::: other.toLazyList)

  def flatMap[B](f: A => NonEmptyLazyList[B]): NonEmptyLazyList[B] =
    f(head) ++ tail.flatMap(f.andThen(_.toLazyList))

  def #::[AA >: A](a: AA): NonEmptyLazyList[AA] =
    prepend(a)

  def prepend[AA >: A](a: AA): NonEmptyLazyList[AA] =
    NonEmptyLazyList(a, head #:: tail)

  /**
    * Alias for append
    *
    * {{{
    * scala> import cats.data.NonEmptyLazyList
    * scala> val nell = NonEmptyLazyList.of(1, 2, 3)
    * scala> nell :+ 4
    * res0: cats.data.NonEmptyLazyList[Int] = NonEmptyLazyList(1, 2, 3, 4)
    * }}}
    */
  def :+[AA >: A](a: AA): NonEmptyLazyList[AA] =
    append(a)

  def append[AA >: A](a: AA): NonEmptyLazyList[AA] =
    NonEmptyLazyList(head, tail :+ a)

  /**
    * Alias for concatNell
    *
    * {{{
    * scala> import cats.data.NonEmptyLazyList
    * scala> val nell = NonEmptyLazyList.of(1, 2, 3)
    * scala> nell #::: NonEmptyLazyList.of(4, 5)
    * res0: cats.data.NonEmptyLazyList[Int] = NonEmptyLazyList(1, 2, 3, 4, 5)
    * }}}
    */
  def #:::[AA >: A](other: NonEmptyLazyList[AA]): NonEmptyLazyList[AA] =
    other.concatNel(this)

  /**
    * Remove elements not matching the predicate
    *
    * {{{
    * scala> import cats.data.NonEmptyLazyList
    * scala> val nell = NonEmptyLazyList.of(1, 2, 3, 4, 5)
    * scala> nell.filter(_ < 3)
    * res0: scala.collection.immutable.LazyList[Int] = LazyList(1, 2)
    * }}}
    */
  def filter(p: A => Boolean): LazyList[A] = {
    val ftail = tail.filter(p)
    if (p(head)) head #:: ftail
    else ftail
  }

  /**
    * Remove elements matching the predicate
    *
    * {{{
    * scala> import cats.data.NonEmptyLazyList
    * scala> val nell = NonEmptyLazyList.of(1, 2, 3, 4, 5)
    * scala> nell.filterNot(_ < 3)
    * res0: scala.collection.immutable.LazyList[Int] = LazyList(3, 4, 5)
    * }}}
    */
  def filterNot(p: A => Boolean): LazyList[A] = {
    val ftail = tail.filterNot(p)
    if (p(head)) ftail
    else head #:: ftail
  }

  /**
    * Builds a new `LazyList` by applying a partial function to
    * all the elements from this `NonEmptyLazyList` on which the function is defined
    *
    * {{{
    * scala> import cats.data.NonEmptyLazyList
    * scala> val nell = NonEmptyLazyList.of(1, 2, 3, 4, 5)
    * scala> nell.collect { case v if v < 3 => v }
    * res0: scala.collection.immutable.LazyList[Int] = LazyList(1, 2)
    * scala> nell.collect {
    *      |  case v if v % 2 == 0 => "even"
    *      |  case _ => "odd"
    *      | }
    * res1: scala.collection.immutable.LazyList[String] = LazyList(odd, even, odd, even, odd)
    * }}}
    */
  def collect[B](pf: PartialFunction[A, B]): LazyList[B] =
    if (pf.isDefinedAt(head)) {
      pf.apply(head) #:: tail.collect(pf)
    } else {
      tail.collect(pf)
    }

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
    Foldable[LazyList].foldRight(toLazyList, lb)(f)

  /**
    * Left-associative reduce using f.
    */
  def reduceLeft[AA >: A](f: (AA, AA) => AA): AA =
    tail.foldLeft[AA](head)(f)

  /**
    * Reduce using the `Semigroup` of `AA`.
    */
  def reduce[AA >: A](implicit S: Semigroup[AA]): AA =
    S.combineAllOption(toLazyList).get

  def traverse[G[_], B](f: A => G[B])(implicit G: Applicative[G]): G[NonEmptyLazyList[B]] =
    G.map2Eval(f(head), Always(Traverse[List].traverse(tail)(f)))(NonEmptyLazyList(_, _)).value

  // TODO *** deal with ListBuffer
  def coflatMap[B](f: NonEmptyLazyList[A] => B): NonEmptyLazyList[B] = {
    val buf = ListBuffer.empty[B]
    @tailrec def consume(as: List[A]): List[B] =
      as match {
        case Nil => buf.toList
        case a #:: as =>
          buf += f(NonEmptyLazyList(a, as))
          consume(as)
      }
    NonEmptyLazyList(f(this), consume(tail))
  }

  def ===[AA >: A](o: NonEmptyLazyList[AA])(implicit AA: Eq[AA]): Boolean =
    ((this.head: AA) === o.head) && (this.tail: LazyList[AA]) === o.tail

  def show[AA >: A](implicit AA: Show[AA]): String =
    toLazyList.iterator.map(AA.show).mkString("NonEmptyLazyList(", ", ", ")")

  override def toString: String = s"NonEmpty$toLazyList"

  /**
    * Remove duplicates. Duplicates are checked using `Order[_]` instance.
    */
  //TODO *** deal with ListBuffer
  def distinct[AA >: A](implicit O: Order[AA]): NonEmptyLazyList[AA] = {
    implicit val ord = O.toOrdering

    val buf = ListBuffer.empty[AA]
    tail.foldLeft(TreeSet(head: AA)) { (elementsSoFar, b) =>
      if (elementsSoFar(b)) elementsSoFar
      else {
        buf += b; elementsSoFar + b
      }
    }

    NonEmptyLazyList(head, buf.toList)
  }

  /**
    * Reverse this `NonEmptyLazyList`.
    *
    * {{{
    * scala> import cats.data.NonEmptyLazyList
    * scala> val nell = NonEmptyLazyList.of(1, 2, 3)
    * scala> nell.reverse
    * res0: cats.data.NonEmptyLazyList[Int] = NonEmptyLazyList(3, 2, 1)
    * }}}
    */
  def reverse: NonEmptyLazyList[A] = {
    @tailrec
    def go(h: A, rest: LazyList[A], acc: LazyList[A]): NonEmptyLazyList[A] =
      rest match {
        case Nil      => NonEmptyLazyList(h, acc)
        case h1 #:: t1 => go(h1, t1, h #:: acc)
      }
    go(head, tail, Nil)
  }

  /**
    * Zips this `NonEmptyLazyList` with another `NonEmptyLazyList` and applies a function for each pair of elements.
    *
    * {{{
    * scala> import cats.data.NonEmptyLazyList
    * scala> val as = NonEmptyLazyList.of(1, 2, 3)
    * scala> val bs = NonEmptyLazyList.of("A", "B", "C")
    * scala> as.zipWith(bs)(_ + _)
    * res0: cats.data.NonEmptyLazyList[String] = NonEmptyLazyList(1A, 2B, 3C)
    * }}}
    */
  def zipWith[B, C](b: NonEmptyLazyList[B])(f: (A, B) => C): NonEmptyLazyList[C] = {
    // TODO *** go back and find all the Nils and make them empty
    @tailrec
    def zwRev(as: LazyList[A], bs: LazyList[B], acc: LazyList[C]): LazyList[C] = (as, bs) match {
      case (Nil, _)           => acc
      case (_, Nil)           => acc
      case (x #:: xs, y #:: ys) => zwRev(xs, ys, f(x, y) #:: acc)
    }

    NonEmptyLazyList(f(head, b.head), zwRev(tail, b.tail, Nil).reverse)
  }

  /**
    * Zips each element of this `NonEmptyLazyList` with its index.
    *
    * {{{
    * scala> import cats.data.NonEmptyLazyList
    * scala> val nell = NonEmptyLazyList.of("a", "b", "c")
    * scala> nell.zipWithIndex
    * res0: cats.data.NonEmptyLazyList[(String, Int)] = NonEmptyLazyList((a,0), (b,1), (c,2))
    * }}}
    */ // TODO *** List builder
  def zipWithIndex: NonEmptyLazyList[(A, Int)] = {
    val bldr = List.newBuilder[(A, Int)]
    var idx = 1
    val it = tail.iterator
    while (it.hasNext) {
      bldr += ((it.next, idx))
      idx += 1
    }
    NonEmptyLazyList((head, 0), bldr.result)
  }

  /**
    * Sorts this `NonEmptyLazyList` according to an `Order` on transformed `B` from `A`
    *
    * {{{
    * scala> import cats.data.NonEmptyLazyList
    * scala> import cats.instances.int._
    * scala> val nell = NonEmptyLazyList.of(('a', 4), ('z', 1), ('e', 22))
    * scala> nell.sortBy(_._2)
    * res0: cats.data.NonEmptyLazyList[(Char, Int)] = NonEmptyLazyList((z,1), (a,4), (e,22))
    * }}}
    */
  def sortBy[B](f: A => B)(implicit B: Order[B]): NonEmptyLazyList[A] =
  // safe: sorting a NonEmptyLazyList cannot produce an empty LazyList
    NonEmptyLazyList.fromLazyListUnsafe(toLazyList.sortBy(f)(B.toOrdering))

  /**
    * Sorts this `NonEmptyLazyList` according to an `Order`
    *
    * {{{
    * scala> import cats.data.NonEmptyLazyList
    * scala> import cats.instances.int._
    * scala> val nell = NonEmptyLazyList.of(12, 4, 3, 9)
    * scala> nell.sorted
    * res0: cats.data.NonEmptyLazyList[Int] = NonEmptyLazyList(3, 4, 9, 12)
    * }}}
    */
  def sorted[AA >: A](implicit AA: Order[AA]): NonEmptyLazyList[AA] =
  // safe: sorting a NonEmptyLazyList cannot produce an empty LazyList
    NonEmptyLazyList.fromLazyListUnsafe(toLazyList.sorted(AA.toOrdering))

  /**
    * Groups elements inside this `NonEmptyLazyList` according to the `Order`
    * of the keys produced by the given mapping function.
    *
    * {{{
    * scala> import scala.collection.immutable.SortedMap
    * scala> import cats.data.NonEmptyLazyList
    * scala> import cats.instances.boolean._
    * scala> val nell = NonEmptyLazyList.of(12, -2, 3, -5)
    * scala> nell.groupBy(_ >= 0)
    * res0: SortedMap[Boolean, cats.data.NonEmptyLazyList[Int]] = Map(false -> NonEmptyLazyList(-2, -5), true -> NonEmptyLazyList(12, 3))
    * }}}
    */ // TODO builder
  def groupBy[B](f: A => B)(implicit B: Order[B]): SortedMap[B, NonEmptyLazyList[A]] = {
    implicit val ordering: Ordering[B] = B.toOrdering
    var m = TreeMap.empty[B, mutable.Builder[A, LazyList[A]]]

    for { elem <- toLazyList } {
      val k = f(elem)

      m.get(k) match {
        case None          => m += ((k, List.newBuilder[A] += elem))
        case Some(builder) => builder += elem
      }
    }

    m.map {
      case (k, v) => (k, NonEmptyLazyList.fromListUnsafe(v.result))
    }: TreeMap[B, NonEmptyLazyList[A]]
  }

  /**
    * Groups elements inside this `NonEmptyLazyList` according to the `Order`
    * of the keys produced by the given mapping function.
    *
    * {{{
    * scala> import cats.data._
    * scala> import cats.instances.boolean._
    * scala> val nell = NonEmptyLazyList.of(12, -2, 3, -5)
    * scala> nell.groupByNem(_ >= 0)
    * res0: NonEmptyMap[Boolean, NonEmptyLazyList[Int]] = Map(false -> NonEmptyLazyList(-2, -5), true -> NonEmptyLazyList(12, 3))
    * }}}
    */
  def groupByNem[B](f: A => B)(implicit B: Order[B]): NonEmptyMap[B, NonEmptyLazyList[A]] =
    NonEmptyMap.fromMapUnsafe(groupBy(f))

  /**
    * Creates new `NonEmptyMap`, similarly to LazyList#toMap from scala standard library.
    *{{{
    * scala> import cats.data._
    * scala> import cats.instances.int._
    * scala> val nell = NonEmptyLazyList((0, "a"), LazyList((1, "b"),(0, "c"), (2, "d")))
    * scala> nell.toNem
    * res0: NonEmptyMap[Int,String] = Map(0 -> c, 1 -> b, 2 -> d)
    *}}}
    *
    */ // TODO *** not lazy. probably ok though
  def toNem[T, U](implicit ev: A <:< (T, U), order: Order[T]): NonEmptyMap[T, U] =
    NonEmptyMap.fromMapUnsafe(SortedMap(toLazyList.map(ev): _*)(order.toOrdering))

  /**
    * Creates new `NonEmptySet`, similarly to LazyList#toSet from scala standard library.
    *{{{
    * scala> import cats.data._
    * scala> import cats.instances.int._
    * scala> val nell = NonEmptyLazyList(1, LazyList(2,2,3,4))
    * scala> nell.toNes
    * res0: cats.data.NonEmptySet[Int] = TreeSet(1, 2, 3, 4)
    *}}}
    */
  def toNes[B >: A](implicit order: Order[B]): NonEmptySet[B] =
    NonEmptySet.of(head, tail: _*)
}

object NonEmptyLazyList extends NonEmptyLazyListInstances {

  def of[A](head: A, tail: A*): NonEmptyLazyList[A] = NonEmptyLazyList(head, tail.toLazyList)

  def ofInitLast[A](init: LazyList[A], last: A): NonEmptyLazyList[A] =
    init match {
      case Nil    => NonEmptyLazyList(last, Nil)
      case h #:: t => NonEmptyLazyList(h, t :+ last)
    }

  def one[A](head: A): NonEmptyLazyList[A] = NonEmptyLazyList(head, Nil)

  /**
    * Create a `NonEmptyLazyList` from a `LazyList`.
    *
    * The result will be `None` if the input list is empty and `Some` wrapping a
    * `NonEmptyLazyList` otherwise.
    *
    * @see [[fromListUnsafe]] for an unsafe version that throws an exception if
    * the input list is empty.
    */
  def fromLazyList[A](l: LazyList[A]): Option[NonEmptyLazyList[A]] =
    l match {
      case Nil    => None
      case h #:: t => Some(NonEmptyLazyList(h, t))
    }

  /**
    * Create a `NonEmptyLazyList` from a `LazyList`, or throw an
    * `IllegalArgumentException` if the input list is empty.
    *
    * @see [[fromLazyList]] for a safe version that returns `None` if the input list
    * is empty.
    */
  def fromLazyListUnsafe[A](l: LazyList[A]): NonEmptyLazyList[A] =
    l match {
      case Nil    => throw new IllegalArgumentException("Cannot create NonEmptyLazyList from empty LazyList")
      case h #:: t => NonEmptyLazyList(h, t)
    }

  // TODO *** toList
  def fromFoldable[F[_], A](fa: F[A])(implicit F: Foldable[F]): Option[NonEmptyLazyList[A]] =
    fromLazyList(F.toList(fa))

  def fromReducible[F[_], A](fa: F[A])(implicit F: Reducible[F]): NonEmptyLazyList[A] =
    F.toNonEmptyLazyList(fa)

  class ZipNonEmptyLazyList[A](val value: NonEmptyLazyList[A]) extends AnyVal

  object ZipNonEmptyLazyList {

    def apply[A](nev: NonEmptyLazyList[A]): ZipNonEmptyLazyList[A] =
      new ZipNonEmptyLazyList(nev)

    implicit val catsDataCommutativeApplyForZipNonEmptyLazyList: CommutativeApply[ZipNonEmptyLazyList] =
      new CommutativeApply[ZipNonEmptyLazyList] {
        def ap[A, B](ff: ZipNonEmptyLazyList[A => B])(fa: ZipNonEmptyLazyList[A]): ZipNonEmptyLazyList[B] =
          ZipNonEmptyLazyList(ff.value.zipWith(fa.value)(_.apply(_)))

        override def map[A, B](fa: ZipNonEmptyLazyList[A])(f: (A) => B): ZipNonEmptyLazyList[B] =
          ZipNonEmptyLazyList(fa.value.map(f))

        override def product[A, B](fa: ZipNonEmptyLazyList[A], fb: ZipNonEmptyLazyList[B]): ZipNonEmptyLazyList[(A, B)] =
          ZipNonEmptyLazyList(fa.value.zipWith(fb.value) { case (a, b) => (a, b) })
      }

    implicit def zipNelEq[A: Eq]: Eq[ZipNonEmptyLazyList[A]] = Eq.by(_.value)
  }
}

sealed abstract private[data] class NonEmptyLazyListInstances extends NonEmptyLazyListInstances0 {

  implicit val catsDataInstancesForNonEmptyLazyList: SemigroupK[NonEmptyLazyList]
    with Reducible[NonEmptyLazyList]
    with Bimonad[NonEmptyLazyList]
    with NonEmptyTraverse[NonEmptyLazyList] =
    new NonEmptyReducible[NonEmptyLazyList, LazyList] with SemigroupK[NonEmptyLazyList] with Bimonad[NonEmptyLazyList]
      with NonEmptyTraverse[NonEmptyLazyList] {

      def combineK[A](a: NonEmptyLazyList[A], b: NonEmptyLazyList[A]): NonEmptyLazyList[A] =
        a.concatNel(b)

      override def split[A](fa: NonEmptyLazyList[A]): (A, LazyList[A]) = (fa.head, fa.tail)

      override def reduceLeft[A](fa: NonEmptyLazyList[A])(f: (A, A) => A): A =
        fa.reduceLeft(f)

      override def reduce[A](fa: NonEmptyLazyList[A])(implicit A: Semigroup[A]): A =
        fa.reduce

      override def map[A, B](fa: NonEmptyLazyList[A])(f: A => B): NonEmptyLazyList[B] =
        fa.map(f)

      def pure[A](x: A): NonEmptyLazyList[A] =
        NonEmptyLazyList.one(x)

      def flatMap[A, B](fa: NonEmptyLazyList[A])(f: A => NonEmptyLazyList[B]): NonEmptyLazyList[B] =
        fa.flatMap(f)

      def coflatMap[A, B](fa: NonEmptyLazyList[A])(f: NonEmptyLazyList[A] => B): NonEmptyLazyList[B] =
        fa.coflatMap(f)

      def extract[A](fa: NonEmptyLazyList[A]): A = fa.head

      def nonEmptyTraverse[G[_], A, B](nell: NonEmptyLazyList[A])(f: A => G[B])(implicit G: Apply[G]): G[NonEmptyLazyList[B]] =
        Foldable[List]
          .reduceRightToOption[A, G[List[B]]](nell.tail)(a => G.map(f(a))(_ #:: Nil)) { (a, lglb) =>
          G.map2Eval(f(a), lglb)(_ #:: _)
        }
          .map {
            case None        => G.map(f(nell.head))(NonEmptyLazyList(_, Nil))
            case Some(gtail) => G.map2(f(nell.head), gtail)(NonEmptyLazyList(_, _))
          }
          .value

      override def traverse[G[_], A, B](fa: NonEmptyLazyList[A])(f: A => G[B])(implicit G: Applicative[G]): G[NonEmptyLazyList[B]] =
        fa.traverse(f)

      override def foldLeft[A, B](fa: NonEmptyLazyList[A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b)(f)

      override def foldRight[A, B](fa: NonEmptyLazyList[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        fa.foldRight(lb)(f)

      override def foldMap[A, B](fa: NonEmptyLazyList[A])(f: A => B)(implicit B: Monoid[B]): B =
        B.combineAll(fa.toLazyList.iterator.map(f))

      // TODO *** list buffer
      def tailRecM[A, B](a: A)(f: A => NonEmptyLazyList[Either[A, B]]): NonEmptyLazyList[B] = {
        val buf = new ListBuffer[B]
        @tailrec def go(v: NonEmptyLazyList[Either[A, B]]): Unit = v.head match {
          case Right(b) =>
            buf += b
            NonEmptyLazyList.fromList(v.tail) match {
              case Some(t) => go(t)
              case None    => ()
            }
          case Left(a) => go(f(a) ++ v.tail)
        }
        go(f(a))
        NonEmptyLazyList.fromListUnsafe(buf.result())
      }

      override def fold[A](fa: NonEmptyLazyList[A])(implicit A: Monoid[A]): A =
        fa.reduce

      override def nonEmptyPartition[A, B, C](fa: NonEmptyLazyList[A])(f: (A) => Either[B, C]): Ior[NonEmptyLazyList[B], NonEmptyLazyList[C]] = {
        import cats.syntax.either._

        val reversed = fa.reverse
        val lastIor = f(reversed.head).bimap(NonEmptyLazyList.one, NonEmptyLazyList.one).toIor

        reversed.tail.foldLeft(lastIor)(
          (ior, a) =>
            (f(a), ior) match {
              case (Right(c), Ior.Left(_)) => ior.putRight(NonEmptyLazyList.one(c))
              case (Right(c), _)           => ior.map(c #:: _)
              case (Left(b), Ior.Right(r)) => Ior.bothNel(b, r)
              case (Left(b), _)            => ior.leftMap(b #:: _)
            }
        )

      }

      override def find[A](fa: NonEmptyLazyList[A])(f: A => Boolean): Option[A] =
        fa.find(f)

      override def forall[A](fa: NonEmptyLazyList[A])(p: A => Boolean): Boolean =
        fa.forall(p)

      override def exists[A](fa: NonEmptyLazyList[A])(p: A => Boolean): Boolean =
        fa.exists(p)

      // TODO *** not sure what to do about this
      override def toList[A](fa: NonEmptyLazyList[A]): List[A] = fa.toLazyList.toList

      override def toNonEmptyLazyList[A](fa: NonEmptyLazyList[A]): NonEmptyLazyList[A] = fa

      override def get[A](fa: NonEmptyLazyList[A])(idx: Long): Option[A] =
        if (idx == 0) Some(fa.head) else Foldable[List].get(fa.tail)(idx - 1)
    }

  implicit def catsDataShowForNonEmptyLazyList[A](implicit A: Show[A]): Show[NonEmptyLazyList[A]] =
    Show.show[NonEmptyLazyList[A]](_.show)

  implicit def catsDataSemigroupForNonEmptyLazyList[A]: Semigroup[NonEmptyLazyList[A]] =
    SemigroupK[NonEmptyLazyList].algebra[A]

  implicit def catsDataOrderForNonEmptyLazyList[A](implicit A: Order[A]): Order[NonEmptyLazyList[A]] =
    new NonEmptyLazyListOrder[A] {
      val A0 = A
    }

  implicit def catsDataNonEmptyParallelForNonEmptyLazyList[A]: NonEmptyParallel[NonEmptyLazyList, ZipNonEmptyLazyList] =
    new NonEmptyParallel[NonEmptyLazyList, ZipNonEmptyLazyList] {

      def flatMap: FlatMap[NonEmptyLazyList] = NonEmptyLazyList.catsDataInstancesForNonEmptyLazyList

      def apply: Apply[ZipNonEmptyLazyList] = ZipNonEmptyLazyList.catsDataCommutativeApplyForZipNonEmptyLazyList

      def sequential: ZipNonEmptyLazyList ~> NonEmptyLazyList =
        λ[ZipNonEmptyLazyList ~> NonEmptyLazyList](_.value)

      def parallel: NonEmptyLazyList ~> ZipNonEmptyLazyList =
        λ[NonEmptyLazyList ~> ZipNonEmptyLazyList](nell => new ZipNonEmptyLazyList(nell))
    }
}

sealed abstract private[data] class NonEmptyLazyListInstances0 extends NonEmptyLazyListInstances1 {
  implicit def catsDataPartialOrderForNonEmptyLazyList[A](implicit A: PartialOrder[A]): PartialOrder[NonEmptyLazyList[A]] =
    new NonEmptyLazyListPartialOrder[A] {
      val A0 = A
    }
}

sealed abstract private[data] class NonEmptyLazyListInstances1 {

  implicit def catsDataEqForNonEmptyLazyList[A](implicit A: Eq[A]): Eq[NonEmptyLazyList[A]] =
    new NonEmptyLazyListEq[A] {
      val A0 = A
    }
}

sealed private[data] trait NonEmptyLazyListEq[A] extends Eq[NonEmptyLazyList[A]] {
  implicit def A0: Eq[A]

  override def eqv(x: NonEmptyLazyList[A], y: NonEmptyLazyList[A]): Boolean = x === y
}

sealed private[data] trait NonEmptyLazyListPartialOrder[A] extends PartialOrder[NonEmptyLazyList[A]] with NonEmptyLazyListEq[A] {
  implicit override def A0: PartialOrder[A]

  override def partialCompare(x: NonEmptyLazyList[A], y: NonEmptyLazyList[A]): Double =
    x.toLazyList.partialCompare(y.toLazyList)
}

sealed abstract private[data] class NonEmptyLazyListOrder[A]
  extends Order[NonEmptyLazyList[A]]
    with NonEmptyLazyListPartialOrder[A] {
  implicit override def A0: Order[A]

  override def compare(x: NonEmptyLazyList[A], y: NonEmptyLazyList[A]): Int =
    x.toLazyList.compare(y.toLazyList)
}

