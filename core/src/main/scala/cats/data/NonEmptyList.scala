package cats
package data

import cats.instances.list._
import cats.syntax.eq._

import scala.annotation.tailrec
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

  /**
   * remove elements not matching the predicate
   */
  def filter(p: A => Boolean): List[A] =
    toList.filter(p)

  /**
   * Append another NonEmptyList
   */
  def concat(other: NonEmptyList[A]): NonEmptyList[A] =
    NonEmptyList(head, tail ::: other.toList)

  /**
   * Find the first element matching the predicate, if one exists
   */
  def find(p: A => Boolean): Option[A] =
    toList.find(p)

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

  def coflatMap[B](f: NonEmptyList[A] => B): NonEmptyList[B] = {
    @tailrec def consume(as: List[A], buf: ListBuffer[B]): List[B] =
      as match {
        case Nil => buf.toList
        case a :: as => consume(as, buf += f(NonEmptyList(a, as)))
      }
    NonEmptyList(f(this), consume(this.tail, ListBuffer.empty))
  }

  def ===(o: NonEmptyList[A])(implicit A: Eq[A]): Boolean =
    (this.head === o.head) && this.tail === o.tail

  def show(implicit A: Show[A]): String =
    toList.iterator.map(A.show).mkString("NonEmptyList(", ", ", ")")

  override def toString: String = s"NonEmpty$toList"
}

object NonEmptyList extends NonEmptyListInstances {
  def apply[A](head: A, tail: A*): NonEmptyList[A] = NonEmptyList(head, tail.toList)

  def fromList[A](l: List[A]): Option[NonEmptyList[A]] =
    if (l.isEmpty) None else Some(NonEmptyList(l.head, l.tail))

  def fromListUnsafe[A](l: List[A]): NonEmptyList[A] =
    if (l.nonEmpty) NonEmptyList(l.head, l.tail)
    else throw new IllegalArgumentException("Cannot create NonEmptyList from empty list")
}

private[data] sealed trait NonEmptyListInstances extends NonEmptyListInstances0 {

  implicit val catsDataInstancesForNonEmptyList: SemigroupK[NonEmptyList] with Reducible[NonEmptyList]
      with Comonad[NonEmptyList] with Traverse[NonEmptyList] with MonadRec[NonEmptyList] =
    new NonEmptyReducible[NonEmptyList, List] with SemigroupK[NonEmptyList]
        with Comonad[NonEmptyList] with Traverse[NonEmptyList] with MonadRec[NonEmptyList] {

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

      def traverse[G[_], A, B](fa: NonEmptyList[A])(f: (A) => G[B])(implicit G: Applicative[G]): G[NonEmptyList[B]] =
        G.map2Eval(f(fa.head), Always(Traverse[List].traverse(fa.tail)(f)))(NonEmptyList(_, _)).value

      override def foldLeft[A, B](fa: NonEmptyList[A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b)(f)

      override def foldRight[A, B](fa: NonEmptyList[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        fa.foldRight(lb)(f)

      def tailRecM[A, B](a: A)(f: A => NonEmptyList[A Xor B]): NonEmptyList[B] = {
        val buf = new ListBuffer[B]
        @tailrec def go(v: NonEmptyList[A Xor B]): Unit = v.head match {
            case Xor.Right(b) =>
            buf += b
            NonEmptyList.fromList(v.tail) match {
              case Some(t) => go(t)
              case None => ()
            }
          case Xor.Left(a) => go(f(a) ++ v.tail)
          }
        go(f(a))
        NonEmptyList.fromListUnsafe(buf.result())
      }

      override def forall[A](fa: NonEmptyList[A])(p: A => Boolean): Boolean =
        fa forall p

      override def exists[A](fa: NonEmptyList[A])(p: A => Boolean): Boolean =
        fa exists p

      override def toList[A](fa: NonEmptyList[A]): List[A] = fa.toList
    }

  implicit def catsDataShowForNonEmptyList[A](implicit A: Show[A]): Show[NonEmptyList[A]] =
    Show.show[NonEmptyList[A]](_.show)

  implicit def catsDataSemigroupForNonEmptyList[A]: Semigroup[NonEmptyList[A]] =
    SemigroupK[NonEmptyList].algebra[A]

  implicit def catsDataOrderForNonEmptyList[A:Order]: Order[NonEmptyList[A]] =
    Order.by(_.toList)
}

private[data] sealed trait NonEmptyListInstances0 extends NonEmptyListInstances1 {
  implicit def catsDataPartialOrderForNonEmptyList[A:PartialOrder]: PartialOrder[NonEmptyList[A]] =
    PartialOrder.by(_.toList)
}

private[data] sealed trait NonEmptyListInstances1 {

  implicit def catsDataEqForNonEmptyList[A](implicit A: Eq[A]): Eq[NonEmptyList[A]] =
    new Eq[NonEmptyList[A]] {
      def eqv(x: NonEmptyList[A], y: NonEmptyList[A]): Boolean = x === y
    }
}
