package cats
package data

import scala.annotation.tailrec
import scala.collection.immutable.VectorBuilder
import cats.std.vector._

/**
 * A data type which represents a single element (head) and a Vector
 * (tail).  This can be used to represent a Vector which is guaranteed
 * to not be empty.
 */
final case class NonEmptyVector[A](head: A, tail: Vector[A]) {

  /**
   * Combine the head and tail into a single `Vector[A]` value.
   */
  def unwrap: Vector[A] = head +: tail

  /**
   * remove elements not matching the predicate
   */
  def filter(f: A => Boolean): Vector[A] = unwrap.filter(f)

  /**
   * Append another NonEmptyVector to this
   */
  def combine(other: NonEmptyVector[A]): NonEmptyVector[A] = NonEmptyVector(head, tail ++ other.unwrap)

  /**
   * find the first element matching the predicate, if one exists
   */
  def find(f: A => Boolean): Option[A] = unwrap.find(f)

  /**
   * Check whether at least one element satisfies the predicate.
   */
  def exists(f: A => Boolean): Boolean = unwrap.exists(f)

  /**
   * Check whether all elements satisfy the predicate.
   */
  def forall(f: A => Boolean): Boolean = unwrap.forall(f)

  /**
   * Left-associative fold using f.
   */
  def foldLeft[B](b: B)(f: (B, A) => B): B =
    unwrap.foldLeft(b)(f)

  /**
   * Right-associative fold using f.
   */
  def foldRight[B](lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = 
    Eval.defer(unwrap.foldRight(lb)(f))

  /**
    * Applies f to all the elements
    */
  def map[B](f: A => B): NonEmptyVector[B] =
    NonEmptyVector(f(head), tail.map(f))

  /**
    *  Applies f to all elements and combines the result
    */
  def flatMap[B](f: A => NonEmptyVector[B]): NonEmptyVector[B] = {
    val end = tail.flatMap(a => f(a).unwrap)
    val start = f(head)
    NonEmptyVector(start.head, start.tail ++ end)
  }

  /**
   * Typesafe equality operator.
   *
   * This method is similar to == except that it only allows two
   * NonEmptyVector[A] values to be compared to each other, and uses
   * equality provided by Eq[_] instances, rather than using the
   * universal equality provided by .equals.
   */
  def ===(that: NonEmptyVector[A])(implicit A: Eq[A]): Boolean =
    A.eqv(head, that.head) && Eq[Vector[A]].eqv(tail, that.tail)

  /**
   * Typesafe stringification method.
   *
   * This method is similar to .toString except that it stringifies
   * values according to Show[_] instances, rather than using the
   * universal .toString method.
   */
  def show(implicit A: Show[A]): String =
    s"NonEmptyVector(${A.show(head)}, ${Show[Vector[A]].show(tail)})"
}

private[data] sealed trait NonEmptyVectorInstances extends NonEmptyVectorLowPriority2 {

  implicit def catsDataEqForNonEmptyVector[A](implicit A: Eq[A]): Eq[NonEmptyVector[A]] =
    new Eq[NonEmptyVector[A]]{
      def eqv(x: NonEmptyVector[A], y: NonEmptyVector[A]): Boolean = x === y
    }

  implicit def catsDataShowForNonEmptyVector[A](implicit A: Show[A]): Show[NonEmptyVector[A]] =
    Show.show[NonEmptyVector[A]](_.show)

  implicit def catsDataSemigroupKForNonEmptyVector: SemigroupK[NonEmptyVector] =
    new SemigroupK[NonEmptyVector] {
      def combineK[A](a: NonEmptyVector[A], b: NonEmptyVector[A]): NonEmptyVector[A] =
        a combine b
    }

  implicit def catsDataSemigroupForNonEmptyVector[A]: Semigroup[NonEmptyVector[A]] =
    catsDataSemigroupKForNonEmptyVector.algebra

  implicit def catsDataReducibleForNonEmptyVector: Reducible[NonEmptyVector] =
    new NonEmptyReducible[NonEmptyVector, Vector] {
      override def split[A](fa: NonEmptyVector[A]): (A, Vector[A]) = (fa.head, fa.tail)

      override def size[A](fa: NonEmptyVector[A]): Long = 1 + fa.tail.size.toLong
    }

  implicit def catsDataMonadForNonEmptyVector: Monad[NonEmptyVector] =
    new Monad[NonEmptyVector] {
      override def map[A, B](fa: NonEmptyVector[A])(f: A => B): NonEmptyVector[B] =
        fa map f

      def pure[A](x: A): NonEmptyVector[A] =
        NonEmptyVector(x, Vector.empty)

      def flatMap[A, B](fa: NonEmptyVector[A])(f: A => NonEmptyVector[B]): NonEmptyVector[B] = 
        fa flatMap f
    }
}

trait NonEmptyVectorLowPriority0 {

  implicit def catsDataComonadForNonEmptyVector: Comonad[NonEmptyVector] = new Comonad[NonEmptyVector] {

    def coflatMap[A, B](fa: NonEmptyVector[A])(f: NonEmptyVector[A] => B): NonEmptyVector[B] = {
        @tailrec def consume(as: Vector[A], buf: VectorBuilder[B]): Vector[B] =
          as match {
            case a +: as => consume(as, buf += f(NonEmptyVector(a, as)))
            case _ => buf.result()
          }
        NonEmptyVector(f(fa), consume(fa.tail, new VectorBuilder[B]))
      }


    def extract[A](fa: NonEmptyVector[A]): A = fa.head

    def map[A, B](fa: NonEmptyVector[A])(f: A => B): NonEmptyVector[B] = fa map f
  }
}

trait NonEmptyVectorLowPriority1 extends NonEmptyVectorLowPriority0 {
  implicit def catsDataFunctorForNonEmptyVector: Functor[NonEmptyVector] =
    new Functor[NonEmptyVector] {
      def map[A, B](fa: NonEmptyVector[A])(f: A => B): NonEmptyVector[B] =
        fa map f
    }

}

trait NonEmptyVectorLowPriority2 extends NonEmptyVectorLowPriority1 {
  implicit def catsDataTraverseForNonEmptyVector: Traverse[NonEmptyVector] =
    new Traverse[NonEmptyVector] {
      def traverse[G[_], A, B](fa: NonEmptyVector[A])(f: (A) => G[B])(implicit G: Applicative[G]): G[NonEmptyVector[B]] =   
      G.map2Eval(f(fa.head), Always(Traverse[Vector].traverse(fa.tail)(f)))(NonEmptyVector(_, _)).value
      

      def foldLeft[A, B](fa: NonEmptyVector[A], b: B)(f: (B, A) => B): B = 
        fa.foldLeft(b)(f)

      def foldRight[A, B](fa: NonEmptyVector[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        fa.foldRight(lb)(f)
    }
}

object NonEmptyVector extends NonEmptyVectorInstances
