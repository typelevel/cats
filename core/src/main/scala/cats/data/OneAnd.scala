package cats
package data

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/**
 * A data type which represents a single element (head) and some other
 * structure (tail). As we have done in package.scala, this can be
 * used to represent a List which is guaranteed to not be empty:
 *
 * {{{
 * type NonEmptyList[A] = OneAnd[A, List]
 * }}}
 */
final case class OneAnd[A, F[_]](head: A, tail: F[A]) {

  /**
   * Combine the head and tail into a single `F[A]` value.
   */
  def unwrap(implicit F: MonadCombine[F]): F[A] =
    F.combine(F.pure(head), tail)

  /**
   * remove elements not matching the predicate
   */
  def filter(f: A => Boolean)(implicit F: MonadCombine[F]): F[A] = {
    val rest = F.filter(tail)(f)
    if (f(head)) F.combine(F.pure(head), rest) else rest
  }

  /**
   * Append another OneAnd to this
   */
  def combine(other: OneAnd[A, F])(implicit F: MonadCombine[F]): OneAnd[A, F] =
    OneAnd(head, F.combine(tail, F.combine(F.pure(other.head), other.tail)))

  /**
   * find the first element matching the predicate, if one exists
   */
  def find(f: A => Boolean)(implicit F: Foldable[F]): Option[A] =
    if (f(head)) Some(head) else F.find(tail)(f)

  /**
   * Check whether at least one element satisfies the predicate.
   */
  def exists(p: A => Boolean)(implicit F: Foldable[F]): Boolean =
    p(head) || F.exists(tail)(p)

  /**
   * Check whether all elements satisfy the predicate.
   */
  def forall(p: A => Boolean)(implicit F: Foldable[F]): Boolean =
    p(head) && F.forall(tail)(p)

  /**
   * Left-associative fold on the structure using f.
   */
  def foldLeft[B](b: B)(f: (B, A) => B)(implicit F: Foldable[F]): B =
    F.foldLeft(tail, f(b, head))(f)

  /**
   * Right-associative fold on the structure using f.
   */
  def foldRight[B](lb: Eval[B])(f: (A, Eval[B]) => Eval[B])(implicit F: Foldable[F]): Eval[B] =
    Eval.defer(f(head, F.foldRight(tail, lb)(f)))

  /**
   * Typesafe equality operator.
   *
   * This method is similar to == except that it only allows two
   * OneAnd[A, F] values to be compared to each other, and uses
   * equality provided by Eq[_] instances, rather than using the
   * universal equality provided by .equals.
   */
  def ===(that: OneAnd[A, F])(implicit A: Eq[A], FA: Eq[F[A]]): Boolean =
    A.eqv(head, that.head) && FA.eqv(tail, that.tail)

  /**
   * Typesafe stringification method.
   *
   * This method is similar to .toString except that it stringifies
   * values according to Show[_] instances, rather than using the
   * universal .toString method.
   */
  def show(implicit A: Show[A], FA: Show[F[A]]): String =
    s"OneAnd(${A.show(head)}, ${FA.show(tail)})"
}

trait OneAndInstances {

  implicit def oneAndEq[A, F[_]](implicit A: Eq[A], FA: Eq[F[A]]): Eq[OneAnd[A, F]] =
    new Eq[OneAnd[A, F]]{
      def eqv(x: OneAnd[A, F], y: OneAnd[A, F]): Boolean = x === y
    }

  implicit def oneAndShow[A, F[_]](implicit A: Show[A], FA: Show[F[A]]): Show[OneAnd[A, F]] =
    Show.show[OneAnd[A, F]](_.show)

  implicit def oneAndFunctor[F[_]](implicit F: Functor[F]): Functor[OneAnd[?, F]] =
    new Functor[OneAnd[?, F]] {
      def map[A, B](fa: OneAnd[A, F])(f: A => B): OneAnd[B, F] =
        OneAnd(f(fa.head), F.map(fa.tail)(f))
    }

  implicit def oneAndSemigroupK[F[_]: MonadCombine]: SemigroupK[OneAnd[?, F]] =
    new SemigroupK[OneAnd[?, F]] {
      def combine[A](a: OneAnd[A, F], b: OneAnd[A, F]): OneAnd[A, F] =
        a combine b
    }

  implicit def oneAndFoldable[F[_]](implicit foldable: Foldable[F]): Foldable[OneAnd[?,F]] =
    new Foldable[OneAnd[?,F]] {
      override def foldLeft[A, B](fa: OneAnd[A, F], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b)(f)
      override def foldRight[A, B](fa: OneAnd[A, F], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        fa.foldRight(lb)(f)
      override def isEmpty[A](fa: OneAnd[A, F]): Boolean = false
    }

  implicit def oneAndMonad[F[_]](implicit monad: MonadCombine[F]): Monad[OneAnd[?, F]] =
    new Monad[OneAnd[?, F]] {
      override def map[A, B](fa: OneAnd[A,F])(f: A => B): OneAnd[B, F] =
        OneAnd(f(fa.head), monad.map(fa.tail)(f))

      def pure[A](x: A): OneAnd[A, F] =
        OneAnd(x, monad.empty)

      def flatMap[A, B](fa: OneAnd[A, F])(f: A => OneAnd[B, F]): OneAnd[B, F] = {
        val end = monad.flatMap(fa.tail) { a =>
          val fa = f(a)
          monad.combine(monad.pure(fa.head), fa.tail)
        }
        val fst = f(fa.head)
        OneAnd(fst.head, monad.combine(fst.tail, end))
      }
    }
}

trait OneAndLowPriority {
  implicit val nelComonad: Comonad[OneAnd[?, List]] =
    new Comonad[OneAnd[?, List]] {

      def coflatMap[A, B](fa: OneAnd[A, List])(f: OneAnd[A, List] => B): OneAnd[B, List] = {
        @tailrec def consume(as: List[A], buf: ListBuffer[B]): List[B] =
          as match {
            case Nil => buf.toList
            case a :: as => consume(as, buf += f(OneAnd(a, as)))
          }
        OneAnd(f(fa), consume(fa.tail, ListBuffer.empty))
      }

      def extract[A](fa: OneAnd[A, List]): A =
        fa.head

      def map[A, B](fa: OneAnd[A, List])(f: A => B): OneAnd[B, List] =
        OneAnd(f(fa.head), fa.tail.map(f))
    }
}

object OneAnd extends OneAndInstances with OneAndLowPriority
