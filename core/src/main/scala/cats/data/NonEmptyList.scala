package cats
package data

import cats.instances.list._
import cats.syntax.order._

import scala.annotation.tailrec
import scala.collection.immutable.TreeSet
import scala.collection.mutable.ListBuffer

/**
 * A data type which represents a non empty list of A, with
 * single element (head) and optional structure (tail).
 */
final case class NonEmptyList[A](head: A, tail: List[A]) {

  /**
   * Return the head and tail into a single list
   */
  def toList: List[A] = head :: tail

  /**
   *  Applies f to all the elements of the structure
   */
  def map[B](f: A => B): NonEmptyList[B] =
    NonEmptyList(f(head), tail.map(f))

  def ++(l: List[A]): NonEmptyList[A] =
    NonEmptyList(head, tail ++ l)

  def flatMap[B](f: A => NonEmptyList[B]): NonEmptyList[B] =
    f(head) ++ tail.flatMap(f andThen (_.toList))

  def ::(a: A): NonEmptyList[A] = NonEmptyList(a, head :: tail)

  /**
   * remove elements not matching the predicate
   */
  def filter(p: A => Boolean): List[A] = {
    val ftail = tail.filter(p)
    if (p(head)) head :: ftail
    else ftail
  }

  /**
   * Append another NonEmptyList
   */
  def concat(other: NonEmptyList[A]): NonEmptyList[A] =
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
  def reduceLeft(f: (A, A) => A): A =
    tail.foldLeft(head)(f)

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

  def ===(o: NonEmptyList[A])(implicit A: Eq[A]): Boolean =
    (this.head === o.head) && this.tail === o.tail

  def show(implicit A: Show[A]): String =
    toList.iterator.map(A.show).mkString("NonEmptyList(", ", ", ")")

  override def toString: String = s"NonEmpty$toList"

  /**
   * Remove duplicates. Duplicates are checked using `Order[_]` instance.
   */
  def distinct(implicit O: Order[A]): NonEmptyList[A] = {
    implicit val ord = O.toOrdering

    val buf = ListBuffer.empty[A]
    tail.foldLeft(TreeSet(head)) { (elementsSoFar, a) =>
      if (elementsSoFar(a)) elementsSoFar else { buf += a; elementsSoFar + a }
    }

    NonEmptyList(head, buf.toList)
  }
}

object NonEmptyList extends NonEmptyListInstances {
  def of[A](head: A, tail: A*): NonEmptyList[A] = NonEmptyList(head, tail.toList)

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

  def fromReducible[F[_], A](fa: F[A])(implicit F: Reducible[F]): NonEmptyList[A] =
    F.toNonEmptyList(fa)
}

private[data] sealed trait NonEmptyListInstances extends NonEmptyListInstances0 {

  implicit val catsDataInstancesForNonEmptyList: SemigroupK[NonEmptyList] with Reducible[NonEmptyList]
      with Comonad[NonEmptyList] with Traverse[NonEmptyList] with Monad[NonEmptyList] with RecursiveTailRecM[NonEmptyList] =
    new NonEmptyReducible[NonEmptyList, List] with SemigroupK[NonEmptyList] with Comonad[NonEmptyList]
      with Traverse[NonEmptyList] with Monad[NonEmptyList] with RecursiveTailRecM[NonEmptyList] {

      def combineK[A](a: NonEmptyList[A], b: NonEmptyList[A]): NonEmptyList[A] =
        a concat b

      override def split[A](fa: NonEmptyList[A]): (A, List[A]) = (fa.head, fa.tail)

      override def reduceLeft[A](fa: NonEmptyList[A])(f: (A, A) => A): A =
        fa.reduceLeft(f)

      override def map[A, B](fa: NonEmptyList[A])(f: A => B): NonEmptyList[B] =
        fa map f

      def pure[A](x: A): NonEmptyList[A] =
        NonEmptyList(x, List.empty)

      def flatMap[A, B](fa: NonEmptyList[A])(f: A => NonEmptyList[B]): NonEmptyList[B] =
        fa flatMap f

      def coflatMap[A, B](fa: NonEmptyList[A])(f: NonEmptyList[A] => B): NonEmptyList[B] =
        fa coflatMap f

      def extract[A](fa: NonEmptyList[A]): A = fa.head

      def traverse[G[_], A, B](fa: NonEmptyList[A])(f: A => G[B])(implicit G: Applicative[G]): G[NonEmptyList[B]] =
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

      override def forall[A](fa: NonEmptyList[A])(p: A => Boolean): Boolean =
        fa forall p

      override def exists[A](fa: NonEmptyList[A])(p: A => Boolean): Boolean =
        fa exists p

      override def toList[A](fa: NonEmptyList[A]): List[A] = fa.toList

      override def toNonEmptyList[A](fa: NonEmptyList[A]): NonEmptyList[A] = fa
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
