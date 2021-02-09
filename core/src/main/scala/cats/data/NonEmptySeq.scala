package cats
package data

import cats.data.NonEmptySeq.ZipNonEmptySeq

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.immutable.{Seq, SortedMap, TreeMap, TreeSet}
import kernel.compat.scalaVersionSpecific._

/**
 * A data type which represents a `Seq` guaranteed to contain at least one element.
 * <br/>
 * Note that the constructor is `private` to prevent accidental construction of an empty
 * `NonEmptySeq`. However, due to https://issues.scala-lang.org/browse/SI-6601, on
 * Scala 2.10, this may be bypassed due to a compiler bug.
 */
final class NonEmptySeq[+A] private (val toSeq: Seq[A]) extends AnyVal with NonEmptyCollection[A, Seq, NonEmptySeq] {

  /**
   * Gets the element at the index, if it exists
   */
  def get(i: Int): Option[A] =
    toSeq.lift(i)

  /**
   * Gets the element at the index, or throws an exception if none exists
   */
  def getUnsafe(i: Int): A = toSeq(i)

  /**
   * Updates the element at the index, if it exists
   */
  def updated[AA >: A](i: Int, a: AA): Option[NonEmptySeq[AA]] =
    if (toSeq.isDefinedAt(i)) Some(new NonEmptySeq(toSeq.updated(i, a))) else None

  /**
   * Updates the element at the index, or throws an `IndexOutOfBoundsException`
   * if none exists (if `i` does not satisfy `0 <= i < length`).
   */
  def updatedUnsafe[AA >: A](i: Int, a: AA): NonEmptySeq[AA] = new NonEmptySeq(toSeq.updated(i, a))

  def head: A = toSeq.head

  def tail: Seq[A] = toSeq.tail

  def last: A = toSeq.last

  def init: Seq[A] = toSeq.init

  final def iterator: Iterator[A] = toSeq.iterator

  /**
   * Remove elements not matching the predicate
   *
   * {{{
   * scala> import cats.data.NonEmptySeq
   * scala> val neSeq = NonEmptySeq.of(1, 2, 3, 4, 5)
   * scala> neSeq.filter(_ < 3)
   * res0: scala.collection.immutable.Seq[Int] = List(1, 2)
   * }}}
   */
  def filter(f: A => Boolean): Seq[A] = toSeq.filter(f)

  /**
   * Remove elements matching the predicate
   *
   * {{{
   * scala> import cats.data.NonEmptySeq
   * scala> val neSeq = NonEmptySeq.of(1, 2, 3, 4, 5)
   * scala> neSeq.filterNot(_ < 3)
   * res0: scala.collection.immutable.Seq[Int] = List(3, 4, 5)
   * }}}
   */
  def filterNot(f: A => Boolean): Seq[A] = toSeq.filterNot(f)

  def collect[B](pf: PartialFunction[A, B]): Seq[B] = toSeq.collect(pf)

  /**
   * Alias for [[concat]]
   */
  def ++[AA >: A](other: Seq[AA]): NonEmptySeq[AA] = concat(other)

  /**
   * Append this NESeq to another NESeq, producing a new `NonEmptySeq`.
   *
   * {{{
   * scala> import cats.data.NonEmptySeq
   * scala> val neSeq = NonEmptySeq.of(1, 2, 3)
   * scala> neSeq ++: NonEmptySeq.of(4, 5)
   * res0: cats.data.NonEmptySeq[Int] = NonEmptySeq(1, 2, 3, 4, 5)
   * }}}
   */
  def ++:[AA >: A](other: NonEmptySeq[AA]): NonEmptySeq[AA] = other.concatNeSeq(this)

  /**
   * Append another `Seq` to this, producing a new `NonEmptySeq`.
   */
  def concat[AA >: A](other: Seq[AA]): NonEmptySeq[AA] = new NonEmptySeq(toSeq ++ other)

  /**
   * Append another `NonEmptySeq` to this, producing a new `NonEmptySeq`.
   */
  def concatNeSeq[AA >: A](other: NonEmptySeq[AA]): NonEmptySeq[AA] = new NonEmptySeq(toSeq ++ other.toSeq)

  /**
   * Append an item to this, producing a new `NonEmptySeq`.
   */
  def append[AA >: A](a: AA): NonEmptySeq[AA] = new NonEmptySeq(toSeq :+ a)

  /**
   * Alias for [[append]]
   */
  def :+[AA >: A](a: AA): NonEmptySeq[AA] = append(a)

  /**
   * Prepend an item to this, producing a new `NonEmptySeq`.
   */
  def prepend[AA >: A](a: AA): NonEmptySeq[AA] = new NonEmptySeq(a +: toSeq)

  /**
   * Prepend a `Seq` to this, producing a new `NonEmptySeq`.
   */
  def prependSeq[AA >: A](Seq: Seq[AA]): NonEmptySeq[AA] =
    new NonEmptySeq(Seq ++ this.toSeq)

  /**
   * Alias for [[prepend]]
   */
  def +:[AA >: A](a: AA): NonEmptySeq[AA] = prepend(a)

  /**
   * Find the first element matching the predicate, if one exists
   */
  def find(f: A => Boolean): Option[A] = toSeq.find(f)

  /**
   * Check whether at least one element satisfies the predicate.
   */
  def exists(f: A => Boolean): Boolean = toSeq.exists(f)

  /**
   * Check whether all elements satisfy the predicate.
   */
  def forall(f: A => Boolean): Boolean = toSeq.forall(f)

  /**
   * Left-associative fold using f.
   */
  def foldLeft[B](b: B)(f: (B, A) => B): B =
    toSeq.foldLeft(b)(f)

  /**
   * Right-associative fold using f.
   */
  def foldRight[B](lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    Foldable[Seq].foldRight(toSeq, lb)(f)

  /**
   * Applies f to all the elements
   */
  def map[B](f: A => B): NonEmptySeq[B] =
    new NonEmptySeq(toSeq.map(f))

  /**
   * Applies f to all elements and combines the result
   */
  def flatMap[B](f: A => NonEmptySeq[B]): NonEmptySeq[B] =
    new NonEmptySeq(toSeq.flatMap(a => f(a).toSeq))

  /**
   * Left-associative reduce using f.
   */
  def reduceLeft[AA >: A](f: (AA, AA) => AA): AA =
    tail.foldLeft(head: AA)(f)

  /**
   * Reduce using the Semigroup of A
   */
  def reduce[AA >: A](implicit S: Semigroup[AA]): AA =
    S.combineAllOption(toSeq).get

  /**
   * Typesafe equality operator.
   *
   * This method is similar to == except that it only allows two
   * NonEmptySeq[A] values to be compared to each other, and uses
   * equality provided by Eq[_] instances, rather than using the
   * universal equality provided by .equals.
   */
  def ===[AA >: A](that: NonEmptySeq[AA])(implicit A: Eq[AA]): Boolean =
    Eq[Seq[AA]].eqv(toSeq, that.toSeq)

  /**
   * Typesafe stringification method.
   *
   * This method is similar to .toString except that it stringifies
   * values according to Show[_] instances, rather than using the
   * universal .toString method.
   */
  def show[AA >: A](implicit AA: Show[AA]): String =
    s"NonEmptySeq(${toSeq.map(Show[AA].show).mkString(", ")})"

  def length: Int = toSeq.length

  override def toString: String = s"NonEmptySeq(${toSeq.map(_.toString).mkString(", ")})"

  /**
   * Remove duplicates. Duplicates are checked using `Order[_]` instance.
   */
  def distinct[AA >: A](implicit O: Order[AA]): NonEmptySeq[AA] = {
    implicit val ord: Ordering[AA] = O.toOrdering

    val buf = Seq.newBuilder[AA]
    tail.foldLeft(TreeSet(head: AA)) { (elementsSoFar, a) =>
      if (elementsSoFar(a)) elementsSoFar
      else {
        buf += a; elementsSoFar + a
      }
    }

    NonEmptySeq(head, buf.result())
  }

  /**
   * Zips this `NonEmptySeq` with another `NonEmptySeq` and applies a function for each pair of elements.
   *
   * {{{
   * scala> import cats.data.NonEmptySeq
   * scala> val as = NonEmptySeq.of(1, 2, 3)
   * scala> val bs = NonEmptySeq.of("A", "B", "C")
   * scala> as.zipWith(bs)(_.toString + _)
   * res0: cats.data.NonEmptySeq[String] = NonEmptySeq(1A, 2B, 3C)
   * }}}
   */
  def zipWith[B, C](b: NonEmptySeq[B])(f: (A, B) => C): NonEmptySeq[C] =
    NonEmptySeq.fromSeqUnsafe(toSeq.lazyZip(b.toSeq).map(f))

  def reverse: NonEmptySeq[A] =
    new NonEmptySeq(toSeq.reverse)

  def zipWithIndex: NonEmptySeq[(A, Int)] =
    new NonEmptySeq(toSeq.zipWithIndex)

  def sortBy[B](f: A => B)(implicit B: Order[B]): NonEmptySeq[A] =
    new NonEmptySeq(toSeq.sortBy(f)(B.toOrdering))

  def sorted[AA >: A](implicit AA: Order[AA]): NonEmptySeq[AA] =
    new NonEmptySeq(toSeq.sorted(AA.toOrdering))

  /**
   * Groups elements inside this `NonEmptySeq` according to the `Order`
   * of the keys produced by the given mapping function.
   *
   * {{{
   * scala> import scala.collection.immutable.SortedMap
   * scala> import cats.data.NonEmptySeq
   * scala> import cats.implicits._
   * scala> val neSeq = NonEmptySeq.of(12, -2, 3, -5)
   * scala> val expectedResult = SortedMap(false -> NonEmptySeq.of(-2, -5), true -> NonEmptySeq.of(12, 3))
   * scala> val result = neSeq.groupBy(_ >= 0)
   * scala> result === expectedResult
   * res0: Boolean = true
   * }}}
   */
  final def groupBy[B](f: A => B)(implicit B: Order[B]): SortedMap[B, NonEmptySeq[A]] = {
    implicit val ordering: Ordering[B] = B.toOrdering
    var m = TreeMap.empty[B, mutable.Builder[A, Seq[A]]]

    for { elem <- toSeq } {
      val k = f(elem)

      m.get(k) match {
        case None          => m += ((k, Seq.newBuilder[A] += elem))
        case Some(builder) => builder += elem
      }
    }

    m.map { case (k, v) =>
      (k, NonEmptySeq.fromSeqUnsafe(v.result()))
    }: TreeMap[B, NonEmptySeq[A]]
  }

  /**
   * Groups elements inside this `NonEmptySeq` according to the `Order`
   * of the keys produced by the given mapping function.
   *
   * {{{
   * scala> import cats.data.{NonEmptyMap, NonEmptySeq}
   * scala> import cats.implicits._
   * scala> val nel = NonEmptySeq.of(12, -2, 3, -5)
   * scala> val expectedResult = NonEmptyMap.of(false -> NonEmptySeq.of(-2, -5), true -> NonEmptySeq.of(12, 3))
   * scala> val result = nel.groupByNem(_ >= 0)
   * scala> result === expectedResult
   * res0: Boolean = true
   * }}}
   */
  final def groupByNem[B](f: A => B)(implicit B: Order[B]): NonEmptyMap[B, NonEmptySeq[A]] =
    NonEmptyMap.fromMapUnsafe(groupBy(f))

  /**
   * Partitions elements in fixed size `NonEmptySeq`s.
   *
   * {{{
   * scala> import cats.data.NonEmptySeq
   * scala> import cats.implicits._
   * scala> val nel = NonEmptySeq.of(12, -2, 3, -5)
   * scala> val expectedResult = List(NonEmptySeq.of(12, -2), NonEmptySeq.of(3, -5))
   * scala> val result = nel.grouped(2)
   * scala> result.toList === expectedResult
   * res0: Boolean = true
   * }}}
   */
  def grouped(size: Int): Iterator[NonEmptySeq[A]] = {
    require(size >= 1, f"size=$size%d, but size must be positive")
    toSeq.grouped(size).map(NonEmptySeq.fromSeqUnsafe)
  }

  /**
   * Creates new `NonEmptyMap`, similarly to List#toMap from scala standard library.
   * {{{
   * scala> import cats.data.{NonEmptyMap, NonEmptySeq}
   * scala> import cats.implicits._
   * scala> import scala.collection.immutable.Seq
   * scala> val neSeq = NonEmptySeq((0, "a"), Seq((1, "b"),(0, "c"), (2, "d")))
   * scala> val expectedResult = NonEmptyMap.of(0 -> "c", 1 -> "b", 2 -> "d")
   * scala> val result = neSeq.toNem
   * scala> result === expectedResult
   * res0: Boolean = true
   * }}}
   */
  final def toNem[T, U](implicit ev: A <:< (T, U), order: Order[T]): NonEmptyMap[T, U] =
    NonEmptyMap.fromMapUnsafe(SortedMap(toSeq.map(ev): _*)(order.toOrdering))

  /**
   * Creates new `NonEmptySet`, similarly to List#toSet from scala standard library.
   * {{{
   * scala> import cats.data._
   * scala> import cats.instances.int._
   * scala> import scala.collection.immutable.Seq
   * scala> val neSeq = NonEmptySeq(1, Seq(2,2,3,4))
   * scala> neSeq.toNes
   * res0: cats.data.NonEmptySet[Int] = TreeSet(1, 2, 3, 4)
   * }}}
   */
  final def toNes[B >: A](implicit order: Order[B]): NonEmptySet[B] =
    NonEmptySet.of(head, tail: _*)
}

@suppressUnusedImportWarningForScalaVersionSpecific
sealed abstract private[data] class NonEmptySeqInstances {

  /**
   * This is not a bug. The declared type of `catsDataInstancesForNonEmptySeq` intentionally ignores
   * `NonEmptyReducible` trait for it not being a typeclass.
   *
   * Also see the discussion: PR #3541 and issue #3069.
   */
  implicit val catsDataInstancesForNonEmptySeq
    : SemigroupK[NonEmptySeq] with Bimonad[NonEmptySeq] with NonEmptyTraverse[NonEmptySeq] with Align[NonEmptySeq] =
    new NonEmptyReducible[NonEmptySeq, Seq]
      with SemigroupK[NonEmptySeq]
      with Bimonad[NonEmptySeq]
      with NonEmptyTraverse[NonEmptySeq]
      with Align[NonEmptySeq] {

      def combineK[A](a: NonEmptySeq[A], b: NonEmptySeq[A]): NonEmptySeq[A] =
        a.concatNeSeq(b)

      override def split[A](fa: NonEmptySeq[A]): (A, Seq[A]) = (fa.head, fa.tail)

      override def size[A](fa: NonEmptySeq[A]): Long = fa.length.toLong

      override def reduceLeft[A](fa: NonEmptySeq[A])(f: (A, A) => A): A =
        fa.reduceLeft(f)

      override def reduce[A](fa: NonEmptySeq[A])(implicit A: Semigroup[A]): A =
        fa.reduce

      override def map[A, B](fa: NonEmptySeq[A])(f: A => B): NonEmptySeq[B] =
        fa.map(f)

      def pure[A](x: A): NonEmptySeq[A] =
        NonEmptySeq.one(x)

      def flatMap[A, B](fa: NonEmptySeq[A])(f: A => NonEmptySeq[B]): NonEmptySeq[B] =
        fa.flatMap(f)

      def coflatMap[A, B](fa: NonEmptySeq[A])(f: NonEmptySeq[A] => B): NonEmptySeq[B] = {
        @tailrec def consume(as: Seq[A], buf: mutable.Builder[B, Seq[B]]): Seq[B] =
          as match {
            case a +: as => consume(as, buf += f(NonEmptySeq(a, as)))
            case _       => buf.result()
          }
        NonEmptySeq(f(fa), consume(fa.tail, Seq.newBuilder[B]))
      }

      def extract[A](fa: NonEmptySeq[A]): A = fa.head

      def nonEmptyTraverse[G[_], A, B](
        nev: NonEmptySeq[A]
      )(f: A => G[B])(implicit G: Apply[G]): G[NonEmptySeq[B]] = {
        def loop(head: A, tail: Seq[A]): Eval[G[NonEmptySeq[B]]] =
          tail.headOption.fold(Eval.now(G.map(f(head))(NonEmptySeq(_, Seq.empty[B]))))(h =>
            G.map2Eval(f(head), Eval.defer(loop(h, tail.tail)))((b, acc) => b +: acc)
          )

        loop(nev.head, nev.tail).value
      }

      override def traverse[G[_], A, B](
        fa: NonEmptySeq[A]
      )(f: (A) => G[B])(implicit G: Applicative[G]): G[NonEmptySeq[B]] =
        G.map2Eval(f(fa.head), Always(Traverse[Seq].traverse(fa.tail)(f)))(NonEmptySeq(_, _)).value

      override def zipWithIndex[A](fa: NonEmptySeq[A]): NonEmptySeq[(A, Int)] =
        fa.zipWithIndex

      override def foldLeft[A, B](fa: NonEmptySeq[A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b)(f)

      override def foldRight[A, B](fa: NonEmptySeq[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        fa.foldRight(lb)(f)

      override def foldMap[A, B](fa: NonEmptySeq[A])(f: A => B)(implicit B: Monoid[B]): B =
        B.combineAll(fa.toSeq.iterator.map(f))

      override def nonEmptyPartition[A, B, C](
        fa: NonEmptySeq[A]
      )(f: (A) => Either[B, C]): Ior[NonEmptyList[B], NonEmptyList[C]] = {
        val reversed = fa.reverse
        val lastIor = f(reversed.head) match {
          case Right(c) => Ior.right(NonEmptyList.one(c))
          case Left(b)  => Ior.left(NonEmptyList.one(b))
        }

        reversed.tail.foldLeft(lastIor)((ior, a) =>
          (f(a), ior) match {
            case (Right(c), Ior.Left(_)) => ior.putRight(NonEmptyList.one(c))
            case (Right(c), _)           => ior.map(c :: _)
            case (Left(b), Ior.Right(r)) => Ior.bothNel(b, r)
            case (Left(b), _)            => ior.leftMap(b :: _)
          }
        )
      }

      override def get[A](fa: NonEmptySeq[A])(idx: Long): Option[A] =
        if (0 <= idx && idx < Int.MaxValue) fa.get(idx.toInt) else None

      def tailRecM[A, B](a: A)(f: A => NonEmptySeq[Either[A, B]]): NonEmptySeq[B] = {
        val buf = Seq.newBuilder[B]
        @tailrec def go(v: NonEmptySeq[Either[A, B]]): Unit =
          v.head match {
            case Right(b) =>
              buf += b
              NonEmptySeq.fromSeq(v.tail) match {
                case Some(t) => go(t)
                case None    => ()
              }
            case Left(a) => go(f(a).concat(v.tail))
          }
        go(f(a))
        NonEmptySeq.fromSeqUnsafe(buf.result())
      }

      override def fold[A](fa: NonEmptySeq[A])(implicit A: Monoid[A]): A =
        fa.reduce

      override def find[A](fa: NonEmptySeq[A])(f: A => Boolean): Option[A] =
        fa.find(f)

      override def forall[A](fa: NonEmptySeq[A])(p: A => Boolean): Boolean =
        fa.forall(p)

      override def exists[A](fa: NonEmptySeq[A])(p: A => Boolean): Boolean =
        fa.exists(p)

      override def toList[A](fa: NonEmptySeq[A]): List[A] = fa.toSeq.toList

      override def toNonEmptyList[A](fa: NonEmptySeq[A]): NonEmptyList[A] =
        NonEmptyList(fa.head, fa.tail.toList)

      def functor: Functor[NonEmptySeq] = this

      def align[A, B](fa: NonEmptySeq[A], fb: NonEmptySeq[B]): NonEmptySeq[Ior[A, B]] =
        NonEmptySeq.fromSeqUnsafe(Align[Seq].align(fa.toSeq, fb.toSeq))

      override def alignWith[A, B, C](fa: NonEmptySeq[A], fb: NonEmptySeq[B])(
        f: Ior[A, B] => C
      ): NonEmptySeq[C] =
        NonEmptySeq.fromSeqUnsafe(Align[Seq].alignWith(fa.toSeq, fb.toSeq)(f))
    }

  implicit def catsDataEqForNonEmptySeq[A](implicit A: Eq[A]): Eq[NonEmptySeq[A]] =
    new Eq[NonEmptySeq[A]] {
      def eqv(x: NonEmptySeq[A], y: NonEmptySeq[A]): Boolean = x === y
    }

  implicit def catsDataShowForNonEmptySeq[A](implicit A: Show[A]): Show[NonEmptySeq[A]] =
    Show.show[NonEmptySeq[A]](_.show)

  implicit def catsDataSemigroupForNonEmptySeq[A]: Semigroup[NonEmptySeq[A]] =
    catsDataInstancesForNonEmptySeq.algebra

  implicit def catsDataParallelForNonEmptySeq: NonEmptyParallel.Aux[NonEmptySeq, ZipNonEmptySeq] =
    new NonEmptyParallel[NonEmptySeq] {
      type F[x] = ZipNonEmptySeq[x]

      def apply: Apply[ZipNonEmptySeq] = ZipNonEmptySeq.catsDataCommutativeApplyForZipNonEmptySeq
      def flatMap: FlatMap[NonEmptySeq] = NonEmptySeq.catsDataInstancesForNonEmptySeq

      def sequential: ZipNonEmptySeq ~> NonEmptySeq =
        new (ZipNonEmptySeq ~> NonEmptySeq) { def apply[A](a: ZipNonEmptySeq[A]): NonEmptySeq[A] = a.value }

      def parallel: NonEmptySeq ~> ZipNonEmptySeq =
        new (NonEmptySeq ~> ZipNonEmptySeq) {
          def apply[A](nev: NonEmptySeq[A]): ZipNonEmptySeq[A] = new ZipNonEmptySeq(nev)
        }
    }

}

object NonEmptySeq extends NonEmptySeqInstances with Serializable {

  def apply[A](head: A, tail: Seq[A]): NonEmptySeq[A] =
    new NonEmptySeq(head +: tail)

  def of[A](head: A, tail: A*): NonEmptySeq[A] = {
    val buf = Seq.newBuilder[A]
    buf += head
    tail.foreach(buf += _)
    new NonEmptySeq(buf.result())
  }

  def one[A](head: A): NonEmptySeq[A] = apply(head, Seq.empty[A])

  def unapply[A](nev: NonEmptySeq[A]): Some[(A, Seq[A])] = Some((nev.head, nev.tail))

  def fromSeq[A](Seq: Seq[A]): Option[NonEmptySeq[A]] =
    if (Seq.isEmpty) None else Some(new NonEmptySeq(Seq))

  def fromSeqUnsafe[A](Seq: Seq[A]): NonEmptySeq[A] =
    if (Seq.nonEmpty) new NonEmptySeq(Seq)
    else throw new IllegalArgumentException("Cannot create NonEmptySeq from empty Seq")

  class ZipNonEmptySeq[A](val value: NonEmptySeq[A]) extends Serializable

  object ZipNonEmptySeq {

    def apply[A](nev: NonEmptySeq[A]): ZipNonEmptySeq[A] =
      new ZipNonEmptySeq(nev)

    implicit val catsDataCommutativeApplyForZipNonEmptySeq: CommutativeApply[ZipNonEmptySeq] =
      new CommutativeApply[ZipNonEmptySeq] {
        def ap[A, B](ff: ZipNonEmptySeq[A => B])(fa: ZipNonEmptySeq[A]): ZipNonEmptySeq[B] =
          ZipNonEmptySeq(ff.value.zipWith(fa.value)(_.apply(_)))

        override def map[A, B](fa: ZipNonEmptySeq[A])(f: (A) => B): ZipNonEmptySeq[B] =
          ZipNonEmptySeq(fa.value.map(f))

        override def product[A, B](fa: ZipNonEmptySeq[A], fb: ZipNonEmptySeq[B]): ZipNonEmptySeq[(A, B)] =
          ZipNonEmptySeq(fa.value.zipWith(fb.value) { case (a, b) => (a, b) })
      }

    @deprecated("Use catsDataEqForZipNonEmptySeq", "2.0.0-RC2")
    private[data] def zipNevEq[A: Eq]: Eq[ZipNonEmptySeq[A]] = catsDataEqForZipNonEmptySeq[A]

    implicit def catsDataEqForZipNonEmptySeq[A: Eq]: Eq[ZipNonEmptySeq[A]] = Eq.by(_.value)
  }
}
