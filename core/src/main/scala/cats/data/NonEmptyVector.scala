package cats
package data

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
final class NonEmptyVector[A] private (val toVector: Vector[A]) extends AnyVal {

  /** Gets the element at the index, if it exists */
  def get(i: Int): Option[A] =
    toVector.lift(i)

  /** Gets the element at the index, or throws an exception if none exists */
  def getUnsafe(i: Int): A = toVector(i)

  /** Updates the element at the index, if it exists */
  def updated(i: Int, a: A): Option[NonEmptyVector[A]] =
    if (toVector.isDefinedAt(i)) Some(new NonEmptyVector(toVector.updated(i, a))) else None

  /**
   * Updates the element at the index, or throws an `IndexOutOfBoundsException`
   * if none exists (if `i` does not satisfy `0 <= i < length`).
   */
  def updatedUnsafe(i: Int, a: A):
      NonEmptyVector[A] = new NonEmptyVector(toVector.updated(i, a))

  def head: A = toVector.head

  def tail: Vector[A] = toVector.tail

  /**
   * remove elements not matching the predicate
   */
  def filter(f: A => Boolean): Vector[A] = toVector.filter(f)

  /**
   * Alias for [[concat]]
   */
  def ++(other: Vector[A]): NonEmptyVector[A] = concat(other)

  /**
   * Append another `Vector` to this, producing a new `NonEmptyVector`.
   */
  def concat(other: Vector[A]): NonEmptyVector[A] = new NonEmptyVector(toVector ++ other)

  /**
   * Append another `NonEmptyVector` to this, producing a new `NonEmptyVector`.
   */
  def concatNev(other: NonEmptyVector[A]): NonEmptyVector[A] = new NonEmptyVector(toVector ++ other.toVector)

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
  def reduceLeft(f: (A, A) => A): A =
    tail.foldLeft(head)(f)

  /**
    * Reduce using the Semigroup of A
    */
  def reduce(implicit S: Semigroup[A]): A =
    S.combineAllOption(toVector).get

  /**
   * Typesafe equality operator.
   *
   * This method is similar to == except that it only allows two
   * NonEmptyVector[A] values to be compared to each other, and uses
   * equality provided by Eq[_] instances, rather than using the
   * universal equality provided by .equals.
   */
  def ===(that: NonEmptyVector[A])(implicit A: Eq[A]): Boolean = Eq[Vector[A]].eqv(toVector, that.toVector)

  /**
   * Typesafe stringification method.
   *
   * This method is similar to .toString except that it stringifies
   * values according to Show[_] instances, rather than using the
   * universal .toString method.
   */
  def show(implicit A: Show[A]): String =
    s"NonEmpty${Show[Vector[A]].show(toVector)}"

  def length: Int = toVector.length

  override def toString: String = s"NonEmpty${toVector.toString}"

  /**
   * Remove duplicates. Duplicates are checked using `Order[_]` instance.
   */
  def distinct(implicit O: Order[A]): NonEmptyVector[A] = {
    implicit val ord = O.toOrdering

    val buf = Vector.newBuilder[A]
    tail.foldLeft(TreeSet(head)) { (elementsSoFar, a) =>
      if (elementsSoFar(a)) elementsSoFar else { buf += a; elementsSoFar + a }
    }

    NonEmptyVector(head, buf.result())
  }
}

private[data] sealed trait NonEmptyVectorInstances {

  implicit val catsDataInstancesForNonEmptyVector: SemigroupK[NonEmptyVector] with Reducible[NonEmptyVector]
    with Comonad[NonEmptyVector] with Traverse[NonEmptyVector] with Monad[NonEmptyVector] with RecursiveTailRecM[NonEmptyVector] =
    new NonEmptyReducible[NonEmptyVector, Vector] with SemigroupK[NonEmptyVector] with Comonad[NonEmptyVector]
      with Traverse[NonEmptyVector] with Monad[NonEmptyVector] with RecursiveTailRecM[NonEmptyVector] {

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
        NonEmptyVector(x, Vector.empty)

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

      def traverse[G[_], A, B](fa: NonEmptyVector[A])(f: (A) => G[B])(implicit G: Applicative[G]): G[NonEmptyVector[B]] =
        G.map2Eval(f(fa.head), Always(Traverse[Vector].traverse(fa.tail)(f)))(NonEmptyVector(_, _)).value


      override def foldLeft[A, B](fa: NonEmptyVector[A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b)(f)

      override def foldRight[A, B](fa: NonEmptyVector[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        fa.foldRight(lb)(f)

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
    }

  implicit def catsDataEqForNonEmptyVector[A](implicit A: Eq[A]): Eq[NonEmptyVector[A]] =
    new Eq[NonEmptyVector[A]]{
      def eqv(x: NonEmptyVector[A], y: NonEmptyVector[A]): Boolean = x === y
    }

  implicit def catsDataShowForNonEmptyVector[A](implicit A: Show[A]): Show[NonEmptyVector[A]] =
    Show.show[NonEmptyVector[A]](_.show)

  implicit def catsDataSemigroupForNonEmptyVector[A]: Semigroup[NonEmptyVector[A]] =
    catsDataInstancesForNonEmptyVector.algebra

}

object NonEmptyVector extends NonEmptyVectorInstances {

  def apply[A](head: A, tail: Vector[A]): NonEmptyVector[A] =
    new NonEmptyVector(head +: tail)

  def of[A](head: A, tail: A*): NonEmptyVector[A] = {
    val buf = Vector.newBuilder[A]
    buf += head
    tail.foreach(buf += _)
    new NonEmptyVector(buf.result)
  }

  def unapply[A](nev: NonEmptyVector[A]): Some[(A, Vector[A])] = Some((nev.head, nev.tail))

  def fromVector[A](vector: Vector[A]): Option[NonEmptyVector[A]] =
    if (vector.isEmpty) None else Some(new NonEmptyVector(vector))

  def fromVectorUnsafe[A](vector: Vector[A]): NonEmptyVector[A] =
    if (vector.nonEmpty) new NonEmptyVector(vector)
    else throw new IllegalArgumentException("Cannot create NonEmptyVector from empty vector")
}
