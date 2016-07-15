package cats
package data

import scala.annotation.tailrec
import scala.collection.immutable.VectorBuilder
import cats.instances.vector._

/**
 * A data type which represents a non empty Vector.
 */
final case class NonEmptyVector[A] private (toVector: Vector[A]) {

  /** Gets the element at the index, if it exists */
  def get(i: Int): Option[A] =
    toVector.lift(i)

  /** Gets the element at the index, or throws an exception if none exists */
  def unsafeGet(i: Int): A = toVector(i)

  /** Updates the element at the index, if it exists */
  def updated(i: Int, a: A): Option[NonEmptyVector[A]] =
    if (toVector.isDefinedAt(i)) Some(NonEmptyVector(toVector.updated(i, a))) else None

  /** Updates the element at the index, or throws an exeption if none exists */
  def unsafeUpdated(i: Int, a: A):
      NonEmptyVector[A] = NonEmptyVector(toVector.updated(i, a))

  def head: A = toVector.head

  def tail: Vector[A] = toVector.tail

  /**
   * remove elements not matching the predicate
   */
  def filter(f: A => Boolean): Vector[A] = toVector.filter(f)

  /**
   * Append another NonEmptyVector to this
   */
  def concat(other: NonEmptyVector[A]): NonEmptyVector[A] = NonEmptyVector(toVector ++ other.toVector)

  /**
    * Alias for concat
    */
  def ++(other: NonEmptyVector[A]): NonEmptyVector[A] = concat(other)

  /**
   * Append another Vector to this
   */
  def concat(other: Vector[A]): NonEmptyVector[A] = NonEmptyVector(toVector ++ other)

  /**
    * Alias for concat
    */
  def ++(other: Vector[A]): NonEmptyVector[A] = concat(other)

  /**
   * find the first element matching the predicate, if one exists
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
    NonEmptyVector(toVector.map(f))

  /**
    *  Applies f to all elements and combines the result
    */
  def flatMap[B](f: A => NonEmptyVector[B]): NonEmptyVector[B] =
    NonEmptyVector(toVector.flatMap(a => f(a).toVector))

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
    s"NonEmptyVector(${Show[Vector[A]].show(toVector)})"
}

private[data] sealed trait NonEmptyVectorInstances {

  implicit val catsDataInstancesForNonEmptyVector: SemigroupK[NonEmptyVector] with Reducible[NonEmptyVector]
      with Comonad[NonEmptyVector] with Traverse[NonEmptyVector] with MonadRec[NonEmptyVector] =
    new NonEmptyReducible[NonEmptyVector, Vector] with SemigroupK[NonEmptyVector]
        with Comonad[NonEmptyVector] with Traverse[NonEmptyVector] with MonadRec[NonEmptyVector] {

      def combineK[A](a: NonEmptyVector[A], b: NonEmptyVector[A]): NonEmptyVector[A] =
        a concat b

      override def split[A](fa: NonEmptyVector[A]): (A, Vector[A]) = (fa.head, fa.tail)

      override def size[A](fa: NonEmptyVector[A]): Long = 1 + fa.tail.size.toLong

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

      def tailRecM[A, B](a: A)(f: A => NonEmptyVector[A Xor B]): NonEmptyVector[B] = {
        val buf = new VectorBuilder[B]
        @tailrec def go(v: NonEmptyVector[A Xor B]): Unit = v.head match {
            case Xor.Right(b) =>
            buf += b
            NonEmptyVector.fromVector(v.tail) match {
              case Some(t) => go(t)
              case None => ()
            }
          case Xor.Left(a) => go(f(a).concat(v.tail))
          }
        go(f(a))
        NonEmptyVector.unsafeFromVector(buf.result())
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
    NonEmptyVector(head +: tail)

  def apply[A](head: A, tail: A*): NonEmptyVector[A] = {
    val buf = Vector.newBuilder[A]
    buf += head
    tail.foreach(buf += _)
    NonEmptyVector(buf.result)
  }

  def fromVector[A](vector: Vector[A]): Option[NonEmptyVector[A]] =
    if (vector.isEmpty) None else Some(new NonEmptyVector(vector))

  def unsafeFromVector[A](vector: Vector[A]): NonEmptyVector[A] =
    if (vector.nonEmpty) NonEmptyVector(vector)
    else throw new IllegalArgumentException("Cannot create NonEmptyVector from empty vector")
}
