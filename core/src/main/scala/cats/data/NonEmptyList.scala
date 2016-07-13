package cats
package data

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
   * remove elements not matching the predicate
   */
  def filter(p: A => Boolean): List[A] =
    toList.filter(p)

  /**
   * Append another NonEmptyList
   */
  def combine(other: NonEmptyList[A]): NonEmptyList[A] =
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
    p(head) && tail.exists(p)

  /**
   * Left-associative fold on the structure using f.
   */
  def foldLeft[B](b: B)(f: (B, A) => B): B =
    tail.foldLeft(f(b, head))(f)

  /**
   * Right-associative fold on the structure using f.
   */
  def foldRight[B](lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    toList.foldRight(lb)(f)

  /**
   *  Applies f to all the elements of the structure
   *  TODO It would be nice to have variance on this particular method def map[AA <: A, B](f: AA => B): NonEmptyList[B] so that you can pass a function for a supertype of A into map. @yilinwei
   * https://github.com/typelevel/cats/pull/1120#discussion-diff-66881573
   */
  def map[B](f: A => B): NonEmptyList[B] =
    NonEmptyList(f(head), tail.map(f))
}

private[data] sealed trait NonEmptyListInstances extends NonEmptyListLowPriority2 {

  def apply[A](head: A, tail: A*): NonEmptyList[A] = NonEmptyList(head, tail.toList)

  implicit def catsDataEqForNonEmptyList[A](implicit A: Eq[A]): Eq[NonEmptyList[A]] =
    new Eq[NonEmptyList[A]] {
      def eqv(x: NonEmptyList[A], y: NonEmptyList[A]): Boolean = x === y
    }

  implicit def catsDataShowForNonEmptyList[A](implicit A: Show[A]): Show[NonEmptyList[A]] =
    Show.show[NonEmptyList[A]](_.show)

  implicit def catsDataSemigroupKForNonEmptyList[A]: SemigroupK[NonEmptyList[?]] =
    new SemigroupK[NonEmptyList[?]] {
      def combineK[A](a: NonEmptyList[A], b: NonEmptyList[A]): NonEmptyList[A] =
        a combine b
    }

  implicit def catsDataSemigroupForNonEmptyList[A]: Semigroup[NonEmptyList[A]] =
    catsDataSemigroupKForNonEmptyList[F].algebra

  implicit def catsDataReducibleForNonEmptyList[A]: Reducible[NonEmptyList[?]] =
    new NonEmptyReducible[NonEmptyList[?]] {
      override def split[A](fa: NonEmptyList[A]): (A, F[A]) = (fa.head, fa.tail)
    }

  implicit def catsDataMonadForNonEmptyList[A]: Monad[NonEmptyList[?]] =
    new Monad[NonEmptyList[?]] {
      override def map[A, B](fa: NonEmptyList[F, A])(f: A => B): NonEmptyList[F, B] =
        fa map f

      def pure[A](x: A): NonEmptyList[F, A] =
        NonEmptyList(x, Nil)

      // TODO Could we move this method (and other type class methods) to NonEmptyList and then reference them in the instances? I think that will make the scaladoc for NonEmptyList a bit nicer. @non
      def flatMap[A, B](fa: NonEmptyList[F, A])(f: A => NonEmptyList[F, B]): NonEmptyList[F, B] = {
        val end = monad.flatMap(fa.tail) { a =>
          val fa = f(a)
          monad.combineK(monad.pure(fa.head), fa.tail)
        }
        val fst = f(fa.head)
        NonEmptyList(fst.head, monad.combineK(fst.tail, end))
        // TODO @yilinwei https://github.com/typelevel/cats/pull/1120#discussion_r66882139
        // val xs = f(head) ++ tail.flatMap(f.andThen(_.toList))
        // NonEmptyList(xs.head, xs.tail)
      }
    }
}

trait NonEmptyListLowPriority0 {
  implicit val nelComonad: Comonad[NonEmptyList[List, ?]] =
    new Comonad[NonEmptyList[List, ?]] {
      def coflatMap[A, B](fa: NonEmptyList[List, A])(f: NonEmptyList[List, A] => B): NonEmptyList[List, B] = {
        @tailrec def consume(as: List[A], buf: ListBuffer[B]): List[B] =
          as match {
            case Nil => buf.toList
            case a :: as => consume(as, buf += f(NonEmptyList(a, as)))
          }
        NonEmptyList(f(fa), consume(fa.tail, ListBuffer.empty))
      }

      def extract[A](fa: NonEmptyList[List, A]): A =
        fa.head

      def map[A, B](fa: NonEmptyList[List, A])(f: A => B): NonEmptyList[List, B] =
        fa map f
    }
}

trait NonEmptyListLowPriority1 extends NonEmptyListLowPriority0 {
  implicit def catsDataFunctorForNonEmptyList[F[_]](implicit F: Functor[F]): Functor[NonEmptyList[F, ?]] =
    new Functor[NonEmptyList[F, ?]] {
      def map[A, B](fa: NonEmptyList[F, A])(f: A => B): NonEmptyList[F, B] =
        fa map f
    }

}

trait NonEmptyListLowPriority2 extends NonEmptyListLowPriority1 {
  implicit def catsDataTraverseForNonEmptyList[F[_]](implicit F: Traverse[F]): Traverse[NonEmptyList[F, ?]] =
    new Traverse[NonEmptyList[F, ?]] {
      def traverse[G[_], A, B](fa: NonEmptyList[F, A])(f: (A) => G[B])(implicit G: Applicative[G]): G[NonEmptyList[F, B]] = {
        G.map2Eval(f(fa.head), Always(F.traverse(fa.tail)(f)))(NonEmptyList(_, _)).value
      }

      def foldLeft[A, B](fa: NonEmptyList[F, A], b: B)(f: (B, A) => B): B = {
        fa.foldLeft(b)(f)
      }

      def foldRight[A, B](fa: NonEmptyList[F, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
        fa.foldRight(lb)(f)
      }
    }
}

object NonEmptyList extends NonEmptyListInstances
