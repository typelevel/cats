package cats
package data

import cats.data.NonEmptyVector.ZipNonEmptyVector
import scala.annotation.tailrec
import scala.collection.immutable.{TreeSet, VectorBuilder}
import cats.instances.vector._

/**
 * A data type which represents a `Vector` guaranteed to contain at least one element.
 * <br/>
 * Note that the constructor is `private` to prevent accidental construction of an empty
 * `NonEmptyVector`. However, due to https://issues.scala-lang.org/browse/SI-6601, on
 * Scala 2.10, this may be bypassed due to a compiler bug.
 */
final class NonEmptyVector[+A] private (val toVector: Vector[A]) extends AnyVal {

  /** Gets the element at the index, if it exists */
  def get(i: Int): Option[A] =
    toVector.lift(i)

  /** Gets the element at the index, or throws an exception if none exists */
  def getUnsafe(i: Int): A = toVector(i)

  /** Updates the element at the index, if it exists */
  def updated[AA >: A](i: Int, a: AA): Option[NonEmptyVector[AA]] =
    if (toVector.isDefinedAt(i)) Some(new NonEmptyVector(toVector.updated(i, a))) else None

  /**
   * Updates the element at the index, or throws an `IndexOutOfBoundsException`
   * if none exists (if `i` does not satisfy `0 <= i < length`).
   */
  def updatedUnsafe[AA >: A](i: Int, a: AA):
      NonEmptyVector[AA] = new NonEmptyVector(toVector.updated(i, a))

  def head: A = toVector.head

  def tail: Vector[A] = toVector.tail

  def last: A = toVector.last

  def init: Vector[A] = toVector.init

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
    *  Applies f to all elements and combines the result
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
    implicit val ord = O.toOrdering

    val buf = Vector.newBuilder[AA]
    tail.foldLeft(TreeSet(head: AA)) { (elementsSoFar, a) =>
      if (elementsSoFar(a)) elementsSoFar else { buf += a; elementsSoFar + a }
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
    * scala> as.zipWith(bs)(_ + _)
    * res0: cats.data.NonEmptyVector[String] = NonEmptyVector(1A, 2B, 3C)
    * }}}
    */
  def zipWith[B, C](b: NonEmptyVector[B])(f: (A, B) => C): NonEmptyVector[C] =
    NonEmptyVector.fromVectorUnsafe((toVector, b.toVector).zipped.map(f))

  def reverse: NonEmptyVector[A] =
    new NonEmptyVector(toVector.reverse)

  def zipWithIndex: NonEmptyVector[(A, Int)] =
    new NonEmptyVector(toVector.zipWithIndex)

  def sortBy[B](f: A => B)(implicit B: Order[B]): NonEmptyVector[A] =
    new NonEmptyVector(toVector.sortBy(f)(B.toOrdering))

  def sorted[AA >: A](implicit AA: Order[AA]): NonEmptyVector[AA] =
    new NonEmptyVector(toVector.sorted(AA.toOrdering))
}

private[data] sealed abstract class NonEmptyVectorInstances {

  implicit val catsDataInstancesForNonEmptyVector: SemigroupK[NonEmptyVector] with Reducible[NonEmptyVector]
    with Comonad[NonEmptyVector] with NonEmptyTraverse[NonEmptyVector] with Monad[NonEmptyVector] =
    new NonEmptyReducible[NonEmptyVector, Vector] with SemigroupK[NonEmptyVector] with Comonad[NonEmptyVector]
      with Monad[NonEmptyVector] with NonEmptyTraverse[NonEmptyVector] {

      def combineK[A](a: NonEmptyVector[A], b: NonEmptyVector[A]): NonEmptyVector[A] =
        a concatNev b

      override def split[A](fa: NonEmptyVector[A]): (A, Vector[A]) = (fa.head, fa.tail)

      override def size[A](fa: NonEmptyVector[A]): Long = fa.length.toLong

      override def reduceLeft[A](fa: NonEmptyVector[A])(f: (A, A) => A): A =
        fa.reduceLeft(f)

      override def reduce[A](fa: NonEmptyVector[A])(implicit A: Semigroup[A]): A =
        fa.reduce

      override def map[A, B](fa: NonEmptyVector[A])(f: A => B): NonEmptyVector[B] =
        fa map f

      def pure[A](x: A): NonEmptyVector[A] =
        NonEmptyVector.one(x)

      def flatMap[A, B](fa: NonEmptyVector[A])(f: A => NonEmptyVector[B]): NonEmptyVector[B] =
        fa flatMap f

      def coflatMap[A, B](fa: NonEmptyVector[A])(f: NonEmptyVector[A] => B): NonEmptyVector[B] = {
        @tailrec def consume(as: Vector[A], buf: VectorBuilder[B]): Vector[B] =
          as match {
            case a +: as => consume(as, buf += f(NonEmptyVector(a, as)))
            case _ => buf.result()
          }
        NonEmptyVector(f(fa), consume(fa.tail, new VectorBuilder[B]))
      }

      def extract[A](fa: NonEmptyVector[A]): A = fa.head

      def nonEmptyTraverse[G[_], A, B](nel: NonEmptyVector[A])(f: A => G[B])(implicit G: Apply[G]): G[NonEmptyVector[B]] =
        Foldable[Vector].reduceRightToOption[A, G[Vector[B]]](nel.tail)(a => G.map(f(a))(_ +: Vector.empty)) { (a, lglb) =>
          G.map2Eval(f(a), lglb)(_ +: _)
        }.map {
          case None => G.map(f(nel.head))(NonEmptyVector(_, Vector.empty))
          case Some(gtail) => G.map2(f(nel.head), gtail)(NonEmptyVector(_, _))
        }.value

      override def traverse[G[_], A, B](fa: NonEmptyVector[A])(f: (A) => G[B])(implicit G: Applicative[G]): G[NonEmptyVector[B]] =
        G.map2Eval(f(fa.head), Always(Traverse[Vector].traverse(fa.tail)(f)))(NonEmptyVector(_, _)).value

      override def zipWithIndex[A](fa: NonEmptyVector[A]): NonEmptyVector[(A, Int)] =
        fa.zipWithIndex

      override def foldLeft[A, B](fa: NonEmptyVector[A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b)(f)

      override def foldRight[A, B](fa: NonEmptyVector[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        fa.foldRight(lb)(f)

      override def foldMap[A, B](fa: NonEmptyVector[A])(f: A => B)(implicit B: Monoid[B]): B =
        B.combineAll(fa.toVector.iterator.map(f))

      override def nonEmptyPartition[A, B, C](fa: NonEmptyVector[A])(f: (A) => Either[B, C]): Ior[NonEmptyList[B], NonEmptyList[C]] = {
        import cats.syntax.either._
        import cats.syntax.reducible._

        reduceLeftTo(fa)(a => f(a).bimap(NonEmptyVector.one, NonEmptyVector.one).toIor)((ior, a) => (f(a), ior) match {
          case (Right(c), Ior.Left(_)) => ior.putRight(NonEmptyVector.one(c))
          case (Right(c), _) => ior.map(_ :+ c)
          case (Left(b), Ior.Right(_)) => ior.putLeft(NonEmptyVector.one(b))
          case (Left(b), _) => ior.leftMap(_ :+ b)
        }).bimap(_.toNonEmptyList, _.toNonEmptyList)

      }

      override def get[A](fa: NonEmptyVector[A])(idx: Long): Option[A] =
        if (0 <= idx && idx < Int.MaxValue) fa.get(idx.toInt) else None

      def tailRecM[A, B](a: A)(f: A => NonEmptyVector[Either[A, B]]): NonEmptyVector[B] = {
        val buf = new VectorBuilder[B]
        @tailrec def go(v: NonEmptyVector[Either[A, B]]): Unit = v.head match {
            case Right(b) =>
            buf += b
            NonEmptyVector.fromVector(v.tail) match {
              case Some(t) => go(t)
              case None => ()
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
    }

  implicit def catsDataEqForNonEmptyVector[A](implicit A: Eq[A]): Eq[NonEmptyVector[A]] =
    new Eq[NonEmptyVector[A]]{
      def eqv(x: NonEmptyVector[A], y: NonEmptyVector[A]): Boolean = x === y
    }

  implicit def catsDataShowForNonEmptyVector[A](implicit A: Show[A]): Show[NonEmptyVector[A]] =
    Show.show[NonEmptyVector[A]](_.show)

  implicit def catsDataSemigroupForNonEmptyVector[A]: Semigroup[NonEmptyVector[A]] =
    catsDataInstancesForNonEmptyVector.algebra

  implicit def catsDataParallelForNonEmptyVector[A]: NonEmptyParallel[NonEmptyVector, ZipNonEmptyVector] =
    new NonEmptyParallel[NonEmptyVector, ZipNonEmptyVector] {

      def apply: Apply[ZipNonEmptyVector] = ZipNonEmptyVector.catsDataCommutativeApplyForZipNonEmptyVector
      def flatMap: FlatMap[NonEmptyVector] = NonEmptyVector.catsDataInstancesForNonEmptyVector

      def sequential: ZipNonEmptyVector ~> NonEmptyVector =
        λ[ZipNonEmptyVector ~> NonEmptyVector](_.value)

      def parallel: NonEmptyVector ~> ZipNonEmptyVector =
        λ[NonEmptyVector ~> ZipNonEmptyVector](nev => new ZipNonEmptyVector(nev))
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
          ZipNonEmptyVector(ff.value.zipWith(fa.value)(_ apply _))

        override def map[A, B](fa: ZipNonEmptyVector[A])(f: (A) => B): ZipNonEmptyVector[B] =
          ZipNonEmptyVector(fa.value.map(f))

        override def product[A, B](fa: ZipNonEmptyVector[A], fb: ZipNonEmptyVector[B]): ZipNonEmptyVector[(A, B)] =
          ZipNonEmptyVector(fa.value.zipWith(fb.value){ case (a, b) => (a, b) })
      }

    implicit def zipNevEq[A: Eq]: Eq[ZipNonEmptyVector[A]] = Eq.by(_.value)
  }
}
