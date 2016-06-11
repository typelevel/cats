package cats
package data

import cats.std.list._
//it needs a Functor
//it needs semigroup - combine, filter

/**
  *  A data type which represents a single element (head) and some other
  * structure (tail).
  */
final case class NonEmptyList[A](head: A, tail: List[A]) {

  /**
    * Return the head and tail into a single list
    */
  def unwrap: List[A] = head :: tail

  /**
    * remove elements not matching the predicate
    */
  def filter(p: A => Boolean): List[A] = {
    val rest = tail.filter(p)
    if(p(head)) head::rest else rest
  }

  /**
    * Append another NonEmptyList
    */
  def combine(other: NonEmptyList[A]):NonEmptyList[A] = 
    NonEmptyList(head, MonadCombine[List].combineK(tail, other.head::other.tail))

  /**
    * Find the first element matching the predicate, if one exists
    */
  def find(p:A=>Boolean): Option[A] =
    if(p(head)) Some(head) else tail.find(p)

  /**
    * Check whether at least one element satisfies the predicate
    */
  def exists(p: A => Boolean): Boolean =
    p(head) || tail.exists(p)

  /**
    * Check whether all elements satisfy the predicate
    */
  def forall(p: A => Boolean): Boolean =
    p(head) && tail.exists(p)

  /**
    * Left-associative fold on the structure using f.
    */
  def foldLeft[B](b: B)(f: (B, A) => B): B =
    (head::tail).foldLeft(b)(f)

  /**
    * Right-associative fold on the structure using f.
    */
  def foldRight[B](lb: Eval[B])(f: (A, Eval[B]) => Eval[B]):Eval[B] =
    (head::tail).foldRight(lb)(f)

  /**
    *  Applies f to all the elements of the structure
    */
  def map[B](f: A => B):NonEmptyList[B] =
    NonEmptyList(f(head), tail.map(f))
}
