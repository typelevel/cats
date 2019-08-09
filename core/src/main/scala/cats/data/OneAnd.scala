package cats
package data

import scala.annotation.tailrec
import scala.collection.mutable.Builder
import cats.instances.crossVersionInstancesForLazyList
import kernel.compat.scalaVersionSpecific._

/**
 * A data type which represents a single element (head) and some other
 * structure (tail). As we have done in package.scala, this can be
 * used to represent a Stream which is guaranteed to not be empty:
 *
 * {{{
 * type NonEmptyStream[A] = OneAnd[Stream, A]
 * }}}
 */
final case class OneAnd[F[_], A](head: A, tail: F[A]) {

  /**
   * Combine the head and tail into a single `F[A]` value.
   */
  def unwrap(implicit F: Alternative[F]): F[A] =
    F.combineK(F.pure(head), tail)

  /**
   * remove elements not matching the predicate
   */
  def filter(f: A => Boolean)(implicit FA: Alternative[F], FM: Monad[F]): F[A] = {
    val rest = FM.flatMap(tail)(a => if (f(a)) FM.pure(a) else FA.empty[A])
    if (f(head)) FA.combineK(FM.pure(head), rest) else rest
  }

  /**
   * Append another OneAnd to this
   */
  def combine(other: OneAnd[F, A])(implicit F: Alternative[F]): OneAnd[F, A] =
    OneAnd(head, F.combineK(tail, F.combineK(F.pure(other.head), other.tail)))

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

  def reduceLeft(f: (A, A) => A)(implicit F: Foldable[F]): A =
    F.foldLeft(tail, head)(f)

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
   * Applies f to all the elements of the structure
   */
  def map[B](f: A => B)(implicit F: Functor[F]): OneAnd[F, B] =
    OneAnd(f(head), F.map(tail)(f))

  /**
   * Modify the context `F` using transformation `f`.
   */
  def mapK[G[_]](f: F ~> G): OneAnd[G, A] =
    OneAnd(head, f(tail))

  /**
   * Typesafe equality operator.
   *
   * This method is similar to == except that it only allows two
   * OneAnd[F, A] values to be compared to each other, and uses
   * equality provided by Eq[_] instances, rather than using the
   * universal equality provided by .equals.
   */
  def ===(that: OneAnd[F, A])(implicit A: Eq[A], FA: Eq[F[A]]): Boolean =
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

@suppressUnusedImportWarningForScalaVersionSpecific
sealed abstract private[data] class OneAndInstances extends OneAndLowPriority0 {

  implicit def catsDataParallelForOneAnd[A, M[_]: Alternative, F[_]: Alternative](
    implicit P: Parallel[M, F]
  ): Parallel[OneAnd[M, *], OneAnd[F, *]] =
    new Parallel[OneAnd[M, *], OneAnd[F, *]] {
      def monad: Monad[OneAnd[M, *]] = catsDataMonadForOneAnd(P.monad, Alternative[M])

      def applicative: Applicative[OneAnd[F, *]] = catsDataApplicativeForOneAnd(Alternative[F])

      def sequential: OneAnd[F, *] ~> OneAnd[M, *] =
        λ[OneAnd[F, *] ~> OneAnd[M, *]](ofa => OneAnd(ofa.head, P.sequential(ofa.tail)))

      def parallel: OneAnd[M, *] ~> OneAnd[F, *] =
        λ[OneAnd[M, *] ~> OneAnd[F, *]](ofa => OneAnd(ofa.head, P.parallel(ofa.tail)))

    }

  implicit def catsDataEqForOneAnd[A, F[_]](implicit A: Eq[A], FA: Eq[F[A]]): Eq[OneAnd[F, A]] =
    new Eq[OneAnd[F, A]] {
      def eqv(x: OneAnd[F, A], y: OneAnd[F, A]): Boolean = x === y
    }

  implicit def catsDataShowForOneAnd[A, F[_]](implicit A: Show[A], FA: Show[F[A]]): Show[OneAnd[F, A]] =
    Show.show[OneAnd[F, A]](_.show)

  implicit def catsDataSemigroupKForOneAnd[F[_]: Alternative]: SemigroupK[OneAnd[F, *]] =
    new SemigroupK[OneAnd[F, *]] {
      def combineK[A](a: OneAnd[F, A], b: OneAnd[F, A]): OneAnd[F, A] =
        a.combine(b)
    }

  implicit def catsDataSemigroupForOneAnd[F[_]: Alternative, A]: Semigroup[OneAnd[F, A]] =
    catsDataSemigroupKForOneAnd[F].algebra

  implicit def catsDataMonadForOneAnd[F[_]](implicit monad: Monad[F],
                                            alternative: Alternative[F]): Monad[OneAnd[F, *]] =
    new Monad[OneAnd[F, *]] {
      override def map[A, B](fa: OneAnd[F, A])(f: A => B): OneAnd[F, B] =
        fa.map(f)(monad)

      def pure[A](x: A): OneAnd[F, A] =
        OneAnd(x, alternative.empty)

      def flatMap[A, B](fa: OneAnd[F, A])(f: A => OneAnd[F, B]): OneAnd[F, B] = {
        val end = monad.flatMap(fa.tail) { a =>
          val fa = f(a)
          alternative.combineK(monad.pure(fa.head), fa.tail)
        }
        val fst = f(fa.head)
        OneAnd(fst.head, alternative.combineK(fst.tail, end))
      }

      def tailRecM[A, B](a: A)(fn: A => OneAnd[F, Either[A, B]]): OneAnd[F, B] = {
        def stepF(a: A): F[Either[A, B]] = {
          val oneAnd = fn(a)
          alternative.combineK(monad.pure(oneAnd.head), oneAnd.tail)
        }
        def toFB(in: Either[A, B]): F[B] = in match {
          case Right(b) => monad.pure(b)
          case Left(a)  => monad.tailRecM(a)(stepF)
        }

        // This could probably be in SemigroupK to perform well
        @tailrec
        def combineAll(items: List[F[B]]): F[B] = items match {
          case Nil              => alternative.empty
          case h :: Nil         => h
          case h1 :: h2 :: tail => combineAll(alternative.combineK(h1, h2) :: tail)
        }

        @tailrec
        def go(in: A, rest: List[F[B]]): OneAnd[F, B] =
          fn(in) match {
            case OneAnd(Right(b), tail) =>
              val fbs = monad.flatMap(tail)(toFB)
              OneAnd(b, combineAll(fbs :: rest))
            case OneAnd(Left(a), tail) =>
              val fbs = monad.flatMap(tail)(toFB)
              go(a, fbs :: rest)
          }

        go(a, Nil)
      }
    }
}

sealed abstract private[data] class OneAndLowPriority4 {
  implicit val catsDataComonadForNonEmptyStream: Comonad[OneAnd[LazyList, *]] =
    new Comonad[OneAnd[LazyList, *]] {
      def coflatMap[A, B](fa: OneAnd[LazyList, A])(f: OneAnd[LazyList, A] => B): OneAnd[LazyList, B] = {
        @tailrec def consume(as: LazyList[A], buf: Builder[B, LazyList[B]]): LazyList[B] =
          if (as.isEmpty) buf.result
          else {
            val tail = as.tail
            consume(tail, buf += f(OneAnd(as.head, tail)))
          }
        OneAnd(f(fa), consume(fa.tail, LazyList.newBuilder))
      }

      def extract[A](fa: OneAnd[LazyList, A]): A =
        fa.head

      def map[A, B](fa: OneAnd[LazyList, A])(f: A => B): OneAnd[LazyList, B] =
        fa.map(f)(crossVersionInstancesForLazyList)
    }
}

sealed abstract private[data] class OneAndLowPriority3 extends OneAndLowPriority4 {

  implicit def catsDataFunctorForOneAnd[F[_]](implicit F: Functor[F]): Functor[OneAnd[F, *]] =
    new Functor[OneAnd[F, *]] {
      def map[A, B](fa: OneAnd[F, A])(f: A => B): OneAnd[F, B] =
        fa.map(f)
    }

}

sealed abstract private[data] class OneAndLowPriority2 extends OneAndLowPriority3 {

  implicit def catsDataApplicativeForOneAnd[F[_]](implicit F: Alternative[F]): Applicative[OneAnd[F, *]] =
    new Applicative[OneAnd[F, *]] {
      override def map[A, B](fa: OneAnd[F, A])(f: A => B): OneAnd[F, B] =
        fa.map(f)

      def pure[A](x: A): OneAnd[F, A] =
        OneAnd(x, F.empty)

      override def ap[A, B](ff: OneAnd[F, A => B])(fa: OneAnd[F, A]): OneAnd[F, B] = {
        val (f, tf) = (ff.head, ff.tail)
        val (a, ta) = (fa.head, fa.tail)
        val fb = F.ap(tf)(F.combineK(F.pure(a), ta))
        OneAnd(f(a), F.combineK(F.map(ta)(f), fb))
      }
    }

}

sealed abstract private[data] class OneAndLowPriority1 extends OneAndLowPriority2 {

  implicit def catsDataTraverseForOneAnd[F[_]](implicit F: Traverse[F]): Traverse[OneAnd[F, *]] =
    new Traverse[OneAnd[F, *]] {
      def traverse[G[_], A, B](fa: OneAnd[F, A])(f: (A) => G[B])(implicit G: Applicative[G]): G[OneAnd[F, B]] =
        G.map2Eval(f(fa.head), Always(F.traverse(fa.tail)(f)))(OneAnd(_, _)).value

      def foldLeft[A, B](fa: OneAnd[F, A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b)(f)

      def foldRight[A, B](fa: OneAnd[F, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        fa.foldRight(lb)(f)
    }
}

sealed abstract private[data] class OneAndLowPriority0_5 extends OneAndLowPriority1 {
  implicit def catsDataReducibleForOneAnd[F[_]](implicit F: Foldable[F]): Reducible[OneAnd[F, *]] =
    new NonEmptyReducible[OneAnd[F, *], F] {
      override def split[A](fa: OneAnd[F, A]): (A, F[A]) = (fa.head, fa.tail)

      override def get[A](fa: OneAnd[F, A])(idx: Long): Option[A] =
        if (idx == 0L) Some(fa.head) else F.get(fa.tail)(idx - 1L)

      override def size[A](fa: OneAnd[F, A]): Long = 1 + F.size(fa.tail)
    }
}

sealed abstract private[data] class OneAndLowPriority0 extends OneAndLowPriority0_5 {
  implicit def catsDataNonEmptyTraverseForOneAnd[F[_]](implicit F: Traverse[F],
                                                       F2: Alternative[F]): NonEmptyTraverse[OneAnd[F, *]] =
    new NonEmptyReducible[OneAnd[F, *], F] with NonEmptyTraverse[OneAnd[F, *]] {
      def nonEmptyTraverse[G[_], A, B](fa: OneAnd[F, A])(f: (A) => G[B])(implicit G: Apply[G]): G[OneAnd[F, B]] = {
        import cats.syntax.apply._

        fa.map(a => Apply[G].map(f(a))(OneAnd(_, F2.empty[B])))(F)
          .reduceLeft(((acc, a) => (acc, a).mapN((x: OneAnd[F, B], y: OneAnd[F, B]) => x.combine(y))))
      }

      override def traverse[G[_], A, B](fa: OneAnd[F, A])(f: (A) => G[B])(implicit G: Applicative[G]): G[OneAnd[F, B]] =
        G.map2Eval(f(fa.head), Always(F.traverse(fa.tail)(f)))(OneAnd(_, _)).value

      def split[A](fa: OneAnd[F, A]): (A, F[A]) = (fa.head, fa.tail)
    }
}

object OneAnd extends OneAndInstances
