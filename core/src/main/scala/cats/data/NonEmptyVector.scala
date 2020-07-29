package cats
package data

import cats.data.NonEmptyVector.ZipNonEmptyVector

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.immutable.{SortedMap, TreeMap, TreeSet, VectorBuilder}
import kernel.compat.scalaVersionSpecific._

/**
 * A data type which represents a `Vector` guaranteed to contain at least one element.
 * <br/>
 * Note that the constructor is `private` to prevent accidental construction of an empty
 * `NonEmptyVector`. However, due to https://issues.scala-lang.org/browse/SI-6601, on
 * Scala 2.10, this may be bypassed due to a compiler bug.
 */
final class NonEmptyVector[+A] private (val toVector: Vector[A])
    extends AnyVal
    with NonEmptyCollection[A, Vector, NonEmptyVector] {

  /**
   * Gets the element at the index, if it exists
   */
  def get(i: Int): Option[A] =
    toVector.lift(i)

  /**
   * Gets the element at the index, or throws an exception if none exists
   */
  def getUnsafe(i: Int): A = toVector(i)

  /**
   * Updates the element at the index, if it exists
   */
  def updated[AA >: A](i: Int, a: AA): Option[NonEmptyVector[AA]] =
    if (toVector.isDefinedAt(i)) Some(new NonEmptyVector(toVector.updated(i, a))) else None

  /**
   * Updates the element at the index, or throws an `IndexOutOfBoundsException`
   * if none exists (if `i` does not satisfy `0 <= i < length`).
   */
  def updatedUnsafe[AA >: A](i: Int, a: AA): NonEmptyVector[AA] = new NonEmptyVector(toVector.updated(i, a))

  def head: A = toVector.head

  def tail: Vector[A] = toVector.tail

  def last: A = toVector.last

  def init: Vector[A] = toVector.init

  final def iterator: Iterator[A] = toVector.iterator

  /**
   * Remove elements not matching the predicate
   *
   * {{{
   * scala> import cats.data.NonEmptyVector
   * scala> val nev = NonEmptyVector.of(1, 2, 3, 4, 5)
   * scala> nev.filter(_ < 3)
   * res0: scala.collection.immutable.Vector[Int] = Vector(1, 2)
   * }}}
   */
  def filter(f: A => Boolean): Vector[A] = toVector.filter(f)

  /**
   * Remove elements matching the predicate
   *
   * {{{
   * scala> import cats.data.NonEmptyVector
   * scala> val nev = NonEmptyVector.of(1, 2, 3, 4, 5)
   * scala> nev.filterNot(_ < 3)
   * res0: scala.collection.immutable.Vector[Int] = Vector(3, 4, 5)
   * }}}
   */
  def filterNot(f: A => Boolean): Vector[A] = toVector.filterNot(f)

  def collect[B](pf: PartialFunction[A, B]): Vector[B] = toVector.collect(pf)

  /**
   * Alias for [[concat]]
   */
  def ++[AA >: A](other: Vector[AA]): NonEmptyVector[AA] = concat(other)

  /**
   * Append this NEV to another NEV, producing a new `NonEmptyVector`.
   *
   * {{{
   * scala> import cats.data.NonEmptyVector
   * scala> val nev = NonEmptyVector.of(1, 2, 3)
   * scala> nev ++: NonEmptyVector.of(4, 5)
   * res0: cats.data.NonEmptyVector[Int] = NonEmptyVector(1, 2, 3, 4, 5)
   * }}}
   */
  def ++:[AA >: A](other: NonEmptyVector[AA]): NonEmptyVector[AA] = other.concatNev(this)

  /**
   * Append another `Vector` to this, producing a new `NonEmptyVector`.
   */
  def concat[AA >: A](other: Vector[AA]): NonEmptyVector[AA] = new NonEmptyVector(toVector ++ other)

  /**
   * Append another `NonEmptyVector` to this, producing a new `NonEmptyVector`.
   */
  def concatNev[AA >: A](other: NonEmptyVector[AA]): NonEmptyVector[AA] = new NonEmptyVector(toVector ++ other.toVector)

  /**
   * Append an item to this, producing a new `NonEmptyVector`.
   */
  def append[AA >: A](a: AA): NonEmptyVector[AA] = new NonEmptyVector(toVector :+ a)

  /**
   * Alias for [[append]]
   */
  def :+[AA >: A](a: AA): NonEmptyVector[AA] = append(a)

  /**
   * Prepend an item to this, producing a new `NonEmptyVector`.
   */
  def prepend[AA >: A](a: AA): NonEmptyVector[AA] = new NonEmptyVector(a +: toVector)

  /**
   * Prepend a `Vector` to this, producing a new `NonEmptyVector`.
   */
  def prependVector[AA >: A](vector: Vector[AA]): NonEmptyVector[AA] =
    new NonEmptyVector(vector ++ this.toVector)

  /**
   * Alias for [[prepend]]
   */
  def +:[AA >: A](a: AA): NonEmptyVector[AA] = prepend(a)

  /**
   * Find the first element matching the predicate, if one exists
   */
  def find(f: A => Boolean): Option[A] = toVector.find(f)

  /**
   * Check whether at least one element satisfies the predicate.
   */
  def exists(f: A => Boolean): Boolean = toVector.exists(f)

  /**
   * Check whether all elements satisfy the predicate.
   */
  def forall(f: A => Boolean): Boolean = toVector.forall(f)

  /**
   * Left-associative fold using f.
   */
  def foldLeft[B](b: B)(f: (B, A) => B): B =
    toVector.foldLeft(b)(f)

  /**
   * Right-associative fold using f.
   */
  def foldRight[B](lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    Foldable[Vector].foldRight(toVector, lb)(f)

  /**
   * Applies f to all the elements
   */
  def map[B](f: A => B): NonEmptyVector[B] =
    new NonEmptyVector(toVector.map(f))

  /**
   * Applies f to all elements and combines the result
   */
  def flatMap[B](f: A => NonEmptyVector[B]): NonEmptyVector[B] =
    new NonEmptyVector(toVector.flatMap(a => f(a).toVector))

  /**
   * Left-associative reduce using f.
   */
  def reduceLeft[AA >: A](f: (AA, AA) => AA): AA =
    tail.foldLeft(head: AA)(f)

  /**
   * Reduce using the Semigroup of A
   */
  def reduce[AA >: A](implicit S: Semigroup[AA]): AA =
    S.combineAllOption(toVector).get

  /**
   * Typesafe equality operator.
   *
   * This method is similar to == except that it only allows two
   * NonEmptyVector[A] values to be compared to each other, and uses
   * equality provided by Eq[_] instances, rather than using the
   * universal equality provided by .equals.
   */
  def ===[AA >: A](that: NonEmptyVector[AA])(implicit A: Eq[AA]): Boolean =
    Eq[Vector[AA]].eqv(toVector, that.toVector)

  /**
   * Typesafe stringification method.
   *
   * This method is similar to .toString except that it stringifies
   * values according to Show[_] instances, rather than using the
   * universal .toString method.
   */
  def show[AA >: A](implicit AA: Show[AA]): String =
    s"NonEmpty${Show[Vector[AA]].show(toVector)}"

  def length: Int = toVector.length

  override def toString: String = s"NonEmpty${toVector.toString}"

  /**
   * Remove duplicates. Duplicates are checked using `Order[_]` instance.
   */
  def distinct[AA >: A](implicit O: Order[AA]): NonEmptyVector[AA] = {
    implicit val ord: Ordering[AA] = O.toOrdering

    val buf = Vector.newBuilder[AA]
    tail.foldLeft(TreeSet(head: AA)) { (elementsSoFar, a) =>
      if (elementsSoFar(a)) elementsSoFar
      else {
        buf += a; elementsSoFar + a
      }
    }

    NonEmptyVector(head, buf.result())
  }

  /**
   * Zips this `NonEmptyVector` with another `NonEmptyVector` and applies a function for each pair of elements.
   *
   * {{{
   * scala> import cats.data.NonEmptyVector
   * scala> val as = NonEmptyVector.of(1, 2, 3)
   * scala> val bs = NonEmptyVector.of("A", "B", "C")
   * scala> as.zipWith(bs)(_.toString + _)
   * res0: cats.data.NonEmptyVector[String] = NonEmptyVector(1A, 2B, 3C)
   * }}}
   */
  def zipWith[B, C](b: NonEmptyVector[B])(f: (A, B) => C): NonEmptyVector[C] =
    NonEmptyVector.fromVectorUnsafe(toVector.lazyZip(b.toVector).map(f))

  def reverse: NonEmptyVector[A] =
    new NonEmptyVector(toVector.reverse)

  def zipWithIndex: NonEmptyVector[(A, Int)] =
    new NonEmptyVector(toVector.zipWithIndex)

  def sortBy[B](f: A => B)(implicit B: Order[B]): NonEmptyVector[A] =
    new NonEmptyVector(toVector.sortBy(f)(B.toOrdering))

  def sorted[AA >: A](implicit AA: Order[AA]): NonEmptyVector[AA] =
    new NonEmptyVector(toVector.sorted(AA.toOrdering))

  /**
   * Groups elements inside this `NonEmptyVector` according to the `Order`
   * of the keys produced by the given mapping function.
   *
   * {{{
   * scala> import scala.collection.immutable.SortedMap
   * scala> import cats.data.NonEmptyVector
   * scala> import cats.implicits._
   * scala> val nev = NonEmptyVector.of(12, -2, 3, -5)
   * scala> val expectedResult = SortedMap(false -> NonEmptyVector.of(-2, -5), true -> NonEmptyVector.of(12, 3))
   * scala> val result = nev.groupBy(_ >= 0)
   * scala> result === expectedResult
   * res0: Boolean = true
   * }}}
   */
  final def groupBy[B](f: A => B)(implicit B: Order[B]): SortedMap[B, NonEmptyVector[A]] = {
    implicit val ordering: Ordering[B] = B.toOrdering
    var m = TreeMap.empty[B, mutable.Builder[A, Vector[A]]]

    for { elem <- toVector } {
      val k = f(elem)

      m.get(k) match {
        case None          => m += ((k, Vector.newBuilder[A] += elem))
        case Some(builder) => builder += elem
      }
    }

    m.map {
      case (k, v) => (k, NonEmptyVector.fromVectorUnsafe(v.result))
    }: TreeMap[B, NonEmptyVector[A]]
  }

  /**
   * Groups elements inside this `NonEmptyVector` according to the `Order`
   * of the keys produced by the given mapping function.
   *
   * {{{
   * scala> import cats.data.{NonEmptyMap, NonEmptyVector}
   * scala> import cats.implicits._
   * scala> val nel = NonEmptyVector.of(12, -2, 3, -5)
   * scala> val expectedResult = NonEmptyMap.of(false -> NonEmptyVector.of(-2, -5), true -> NonEmptyVector.of(12, 3))
   * scala> val result = nel.groupByNem(_ >= 0)
   * scala> result === expectedResult
   * res0: Boolean = true
   * }}}
   */
  final def groupByNem[B](f: A => B)(implicit B: Order[B]): NonEmptyMap[B, NonEmptyVector[A]] =
    NonEmptyMap.fromMapUnsafe(groupBy(f))

  /**
   * Creates new `NonEmptyMap`, similarly to List#toMap from scala standard library.
   * {{{
   * scala> import cats.data.{NonEmptyMap, NonEmptyVector}
   * scala> import cats.implicits._
   * scala> val nev = NonEmptyVector((0, "a"), Vector((1, "b"),(0, "c"), (2, "d")))
   * scala> val expectedResult = NonEmptyMap.of(0 -> "c", 1 -> "b", 2 -> "d")
   * scala> val result = nev.toNem
   * scala> result === expectedResult
   * res0: Boolean = true
   * }}}
   */
  final def toNem[T, U](implicit ev: A <:< (T, U), order: Order[T]): NonEmptyMap[T, U] =
    NonEmptyMap.fromMapUnsafe(SortedMap(toVector.map(ev): _*)(order.toOrdering))

  /**
   * Creates new `NonEmptySet`, similarly to List#toSet from scala standard library.
   * {{{
   * scala> import cats.data._
   * scala> import cats.instances.int._
   * scala> val nev = NonEmptyVector(1, Vector(2,2,3,4))
   * scala> nev.toNes
   * res0: cats.data.NonEmptySet[Int] = TreeSet(1, 2, 3, 4)
   * }}}
   */
  final def toNes[B >: A](implicit order: Order[B]): NonEmptySet[B] =
    NonEmptySet.of(head, tail: _*)
}

@suppressUnusedImportWarningForScalaVersionSpecific
sealed abstract private[data] class NonEmptyVectorInstances {

  implicit val catsDataInstancesForNonEmptyVector: NonEmptyReducible[NonEmptyVector, Vector]
    with SemigroupK[NonEmptyVector]
    with Bimonad[NonEmptyVector]
    with NonEmptyTraverse[NonEmptyVector]
    with Align[NonEmptyVector] =
    new NonEmptyReducible[NonEmptyVector, Vector]
      with SemigroupK[NonEmptyVector]
      with Bimonad[NonEmptyVector]
      with NonEmptyTraverse[NonEmptyVector]
      with Align[NonEmptyVector] {

      def combineK[A](a: NonEmptyVector[A], b: NonEmptyVector[A]): NonEmptyVector[A] =
        a.concatNev(b)

      override def split[A](fa: NonEmptyVector[A]): (A, Vector[A]) = (fa.head, fa.tail)

      override def size[A](fa: NonEmptyVector[A]): Long = fa.length.toLong

      override def reduceLeft[A](fa: NonEmptyVector[A])(f: (A, A) => A): A =
        fa.reduceLeft(f)

      override def reduce[A](fa: NonEmptyVector[A])(implicit A: Semigroup[A]): A =
        fa.reduce

      override def map[A, B](fa: NonEmptyVector[A])(f: A => B): NonEmptyVector[B] =
        fa.map(f)

      def pure[A](x: A): NonEmptyVector[A] =
        NonEmptyVector.one(x)

      def flatMap[A, B](fa: NonEmptyVector[A])(f: A => NonEmptyVector[B]): NonEmptyVector[B] =
        fa.flatMap(f)

      def coflatMap[A, B](fa: NonEmptyVector[A])(f: NonEmptyVector[A] => B): NonEmptyVector[B] = {
        @tailrec def consume(as: Vector[A], buf: VectorBuilder[B]): Vector[B] =
          as match {
            case a +: as => consume(as, buf += f(NonEmptyVector(a, as)))
            case _       => buf.result()
          }
        NonEmptyVector(f(fa), consume(fa.tail, new VectorBuilder[B]))
      }

      def extract[A](fa: NonEmptyVector[A]): A = fa.head

      def nonEmptyTraverse[G[_], A, B](
        nev: NonEmptyVector[A]
      )(f: A => G[B])(implicit G: Apply[G]): G[NonEmptyVector[B]] = {
        def loop(head: A, tail: Vector[A]): Eval[G[NonEmptyVector[B]]] =
          tail.headOption.fold(Eval.now(G.map(f(head))(NonEmptyVector(_, Vector.empty[B]))))(h =>
            G.map2Eval(f(head), Eval.defer(loop(h, tail.tail)))((b, acc) => b +: acc)
          )

        loop(nev.head, nev.tail).value
      }

      override def traverse[G[_], A, B](
        fa: NonEmptyVector[A]
      )(f: (A) => G[B])(implicit G: Applicative[G]): G[NonEmptyVector[B]] =
        G.map2Eval(f(fa.head), Always(Traverse[Vector].traverse(fa.tail)(f)))(NonEmptyVector(_, _)).value

      override def zipWithIndex[A](fa: NonEmptyVector[A]): NonEmptyVector[(A, Int)] =
        fa.zipWithIndex

      override def foldLeft[A, B](fa: NonEmptyVector[A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b)(f)

      override def foldRight[A, B](fa: NonEmptyVector[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        fa.foldRight(lb)(f)

      override def foldMap[A, B](fa: NonEmptyVector[A])(f: A => B)(implicit B: Monoid[B]): B =
        B.combineAll(fa.toVector.iterator.map(f))

      override def nonEmptyPartition[A, B, C](
        fa: NonEmptyVector[A]
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

      override def get[A](fa: NonEmptyVector[A])(idx: Long): Option[A] =
        if (0 <= idx && idx < Int.MaxValue) fa.get(idx.toInt) else None

      def tailRecM[A, B](a: A)(f: A => NonEmptyVector[Either[A, B]]): NonEmptyVector[B] = {
        val buf = new VectorBuilder[B]
        @tailrec def go(v: NonEmptyVector[Either[A, B]]): Unit =
          v.head match {
            case Right(b) =>
              buf += b
              NonEmptyVector.fromVector(v.tail) match {
                case Some(t) => go(t)
                case None    => ()
              }
            case Left(a) => go(f(a).concat(v.tail))
          }
        go(f(a))
        NonEmptyVector.fromVectorUnsafe(buf.result())
      }

      override def fold[A](fa: NonEmptyVector[A])(implicit A: Monoid[A]): A =
        fa.reduce

      override def find[A](fa: NonEmptyVector[A])(f: A => Boolean): Option[A] =
        fa.find(f)

      override def forall[A](fa: NonEmptyVector[A])(p: A => Boolean): Boolean =
        fa.forall(p)

      override def exists[A](fa: NonEmptyVector[A])(p: A => Boolean): Boolean =
        fa.exists(p)

      override def toList[A](fa: NonEmptyVector[A]): List[A] = fa.toVector.toList

      override def toNonEmptyList[A](fa: NonEmptyVector[A]): NonEmptyList[A] =
        NonEmptyList(fa.head, fa.tail.toList)

      def functor: Functor[NonEmptyVector] = this

      def align[A, B](fa: NonEmptyVector[A], fb: NonEmptyVector[B]): NonEmptyVector[Ior[A, B]] =
        NonEmptyVector.fromVectorUnsafe(Align[Vector].align(fa.toVector, fb.toVector))

      override def alignWith[A, B, C](fa: NonEmptyVector[A], fb: NonEmptyVector[B])(
        f: Ior[A, B] => C
      ): NonEmptyVector[C] =
        NonEmptyVector.fromVectorUnsafe(Align[Vector].alignWith(fa.toVector, fb.toVector)(f))
    }

  implicit def catsDataEqForNonEmptyVector[A](implicit A: Eq[A]): Eq[NonEmptyVector[A]] =
    new Eq[NonEmptyVector[A]] {
      def eqv(x: NonEmptyVector[A], y: NonEmptyVector[A]): Boolean = x === y
    }

  implicit def catsDataShowForNonEmptyVector[A](implicit A: Show[A]): Show[NonEmptyVector[A]] =
    Show.show[NonEmptyVector[A]](_.show)

  implicit def catsDataSemigroupForNonEmptyVector[A]: Semigroup[NonEmptyVector[A]] =
    catsDataInstancesForNonEmptyVector.algebra

  implicit def catsDataParallelForNonEmptyVector: NonEmptyParallel.Aux[NonEmptyVector, ZipNonEmptyVector] =
    new NonEmptyParallel[NonEmptyVector] {
      type F[x] = ZipNonEmptyVector[x]

      def apply: Apply[ZipNonEmptyVector] = ZipNonEmptyVector.catsDataCommutativeApplyForZipNonEmptyVector
      def flatMap: FlatMap[NonEmptyVector] = NonEmptyVector.catsDataInstancesForNonEmptyVector

      def sequential: ZipNonEmptyVector ~> NonEmptyVector =
        new (ZipNonEmptyVector ~> NonEmptyVector) { def apply[A](a: ZipNonEmptyVector[A]): NonEmptyVector[A] = a.value }

      def parallel: NonEmptyVector ~> ZipNonEmptyVector =
        new (NonEmptyVector ~> ZipNonEmptyVector) {
          def apply[A](nev: NonEmptyVector[A]): ZipNonEmptyVector[A] = new ZipNonEmptyVector(nev)
        }
    }

}

object NonEmptyVector extends NonEmptyVectorInstances with Serializable {

  def apply[A](head: A, tail: Vector[A]): NonEmptyVector[A] =
    new NonEmptyVector(head +: tail)

  def of[A](head: A, tail: A*): NonEmptyVector[A] = {
    val buf = Vector.newBuilder[A]
    buf += head
    tail.foreach(buf += _)
    new NonEmptyVector(buf.result)
  }

  def one[A](head: A): NonEmptyVector[A] = apply(head, Vector.empty[A])

  def unapply[A](nev: NonEmptyVector[A]): Some[(A, Vector[A])] = Some((nev.head, nev.tail))

  def fromVector[A](vector: Vector[A]): Option[NonEmptyVector[A]] =
    if (vector.isEmpty) None else Some(new NonEmptyVector(vector))

  def fromVectorUnsafe[A](vector: Vector[A]): NonEmptyVector[A] =
    if (vector.nonEmpty) new NonEmptyVector(vector)
    else throw new IllegalArgumentException("Cannot create NonEmptyVector from empty vector")

  class ZipNonEmptyVector[A](val value: NonEmptyVector[A]) extends Serializable

  object ZipNonEmptyVector {

    def apply[A](nev: NonEmptyVector[A]): ZipNonEmptyVector[A] =
      new ZipNonEmptyVector(nev)

    implicit val catsDataCommutativeApplyForZipNonEmptyVector: CommutativeApply[ZipNonEmptyVector] =
      new CommutativeApply[ZipNonEmptyVector] {
        def ap[A, B](ff: ZipNonEmptyVector[A => B])(fa: ZipNonEmptyVector[A]): ZipNonEmptyVector[B] =
          ZipNonEmptyVector(ff.value.zipWith(fa.value)(_.apply(_)))

        override def map[A, B](fa: ZipNonEmptyVector[A])(f: (A) => B): ZipNonEmptyVector[B] =
          ZipNonEmptyVector(fa.value.map(f))

        override def product[A, B](fa: ZipNonEmptyVector[A], fb: ZipNonEmptyVector[B]): ZipNonEmptyVector[(A, B)] =
          ZipNonEmptyVector(fa.value.zipWith(fb.value) { case (a, b) => (a, b) })
      }

    @deprecated("Use catsDataEqForZipNonEmptyVector", "2.0.0-RC2")
    private[data] def zipNevEq[A: Eq]: Eq[ZipNonEmptyVector[A]] = catsDataEqForZipNonEmptyVector[A]

    implicit def catsDataEqForZipNonEmptyVector[A: Eq]: Eq[ZipNonEmptyVector[A]] = Eq.by(_.value)
  }
}
