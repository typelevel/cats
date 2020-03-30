package alleycats
package std

import alleycats.compat.scalaVersionSpecific._
import cats.{Always, Applicative, Eval, Foldable, Monad, Monoid, Traverse, TraverseFilter}

import scala.annotation.tailrec

object set extends SetInstances

@suppressUnusedImportWarningForScalaVersionSpecific
trait SetInstances {
  // This method advertises parametricity, but relies on using
  // universal hash codes and equality, which hurts our ability to
  // rely on free theorems.
  //
  // Another problem some people have pointed out is the
  // non-associativity of map when using impure functions. For
  // example, consider the following expressions:
  //
  //   import scala.util.Random
  //
  //   val f = (_: Int) => 1
  //   val g = (_: Int) => Random.nextInt
  //
  //   Set(1, 2, 3).map(f).map(g)
  //   Set(1, 2, 3).map(f andThen g)
  //
  // The first Set will contain one random number, and the second will
  // contain three. Since `g` is not a function (speaking strictly)
  // this would not be considered a law violation, but it still makes
  // people uncomfortable.
  implicit val alleyCatsStdSetMonad: Monad[Set] =
    new Monad[Set] {
      def pure[A](a: A): Set[A] = Set(a)
      override def map[A, B](fa: Set[A])(f: A => B): Set[B] = fa.map(f)
      def flatMap[A, B](fa: Set[A])(f: A => Set[B]): Set[B] = fa.flatMap(f)

      override def map2[A, B, Z](fa: Set[A], fb: Set[B])(f: (A, B) => Z): Set[Z] =
        if (fb.isEmpty) Set.empty[Z] // do O(1) work if fb is empty
        else fa.flatMap(a => fb.map(b => f(a, b))) // already O(1) if fa is empty

      override def map2Eval[A, B, Z](fa: Set[A], fb: Eval[Set[B]])(f: (A, B) => Z): Eval[Set[Z]] =
        if (fa.isEmpty) Eval.now(Set.empty[Z]) // no need to evaluate fb
        else fb.map(fb => map2(fa, fb)(f))

      def tailRecM[A, B](a: A)(f: (A) => Set[Either[A, B]]): Set[B] = {
        val bldr = Set.newBuilder[B]

        @tailrec def go(set: Set[Either[A, B]]): Unit = {
          val lefts = set.foldLeft(Set[A]()) { (memo, either) =>
            either.fold(
              memo + _,
              b => {
                bldr += b
                memo
              }
            )
          }
          if (lefts.isEmpty)
            ()
          else
            go(lefts.flatMap(f))
        }
        go(f(a))
        bldr.result()
      }
    }

  // Since iteration order is not guaranteed for sets, folds and other
  // traversals may produce different results for input sets which
  // appear to be the same.
  implicit val alleyCatsSetTraverse: Traverse[Set] =
    new Traverse[Set] {
      def foldLeft[A, B](fa: Set[A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b)(f)
      def foldRight[A, B](fa: Set[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        Foldable.iterateRight(fa, lb)(f)

      override def foldMap[A, B](fa: Set[A])(f: A => B)(implicit B: Monoid[B]): B =
        B.combineAll(fa.iterator.map(f))

      def traverse[G[_]: Applicative, A, B](sa: Set[A])(f: A => G[B]): G[Set[B]] = {
        val G = Applicative[G]
        foldRight[A, G[Set[B]]](sa, Always(G.pure(Set.empty[B]))) { (a, lglb) =>
          G.map2Eval(f(a), lglb)((b, set) => set + b)
        }.value
      }

      override def get[A](fa: Set[A])(idx: Long): Option[A] = {
        @tailrec
        def go(idx: Int, it: Iterator[A]): Option[A] =
          if (it.hasNext) {
            if (idx == 0) Some(it.next)
            else {
              it.next
              go(idx - 1, it)
            }
          } else None
        if (idx < Int.MaxValue && idx >= 0L) go(idx.toInt, fa.iterator) else None
      }

      override def size[A](fa: Set[A]): Long = fa.size.toLong

      override def exists[A](fa: Set[A])(p: A => Boolean): Boolean =
        fa.exists(p)

      override def forall[A](fa: Set[A])(p: A => Boolean): Boolean =
        fa.forall(p)

      override def isEmpty[A](fa: Set[A]): Boolean = fa.isEmpty

      override def fold[A](fa: Set[A])(implicit A: Monoid[A]): A = A.combineAll(fa)

      override def toList[A](fa: Set[A]): List[A] = fa.toList

      override def reduceLeftOption[A](fa: Set[A])(f: (A, A) => A): Option[A] =
        fa.reduceLeftOption(f)

      override def find[A](fa: Set[A])(f: A => Boolean): Option[A] = fa.find(f)

      override def collectFirst[A, B](fa: Set[A])(pf: PartialFunction[A, B]): Option[B] =
        fa.collectFirst(pf)

      override def collectFirstSome[A, B](fa: Set[A])(f: A => Option[B]): Option[B] =
        fa.collectFirst(Function.unlift(f))
    }

  implicit val alleyCatsSetTraverseFilter: TraverseFilter[Set] =
    new TraverseFilter[Set] {
      val traverse: Traverse[Set] = alleyCatsSetTraverse

      def traverseFilter[G[_], A, B](fa: Set[A])(f: A => G[Option[B]])(implicit G: Applicative[G]): G[Set[B]] =
        traverse
          .foldRight(fa, Eval.now(G.pure(Set.empty[B])))((x, xse) => G.map2Eval(f(x), xse)((i, o) => i.fold(o)(o + _)))
          .value
    }
}
