package alleycats.std

import cats.{Applicative, Eval, Foldable, Monad, Monoid, Traverse, TraverseFilter}
import export._

import scala.annotation.tailrec

@exports
object SetInstances {
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
  @export(Orphan)
  implicit val setMonad: Monad[Set] =
    new Monad[Set] {
      def pure[A](a: A): Set[A] = Set(a)
      override def map[A, B](fa: Set[A])(f: A => B): Set[B] = fa.map(f)
      def flatMap[A, B](fa: Set[A])(f: A => Set[B]): Set[B] = fa.flatMap(f)

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
  @export(Orphan)
  implicit val setTraverse: Traverse[Set] =
    new Traverse[Set] {
      def foldLeft[A, B](fa: Set[A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b)(f)
      def foldRight[A, B](fa: Set[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        Foldable.iterateRight(fa, lb)(f)

      override def foldMap[A, B](fa: Set[A])(f: A => B)(implicit B: Monoid[B]): B =
        B.combineAll(fa.iterator.map(f))

      def traverse[G[_]: Applicative, A, B](sa: Set[A])(f: A => G[B]): G[Set[B]] = {
        val G = Applicative[G]
        sa.foldLeft(G.pure(Set.empty[B])) { (buf, a) =>
          G.map2(buf, f(a))(_ + _)
        }
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
        if (idx < Int.MaxValue && idx >= 0L) go(idx.toInt, fa.toIterator) else None
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

  @export(Orphan)
  implicit val setTraverseFilter: TraverseFilter[Set] =
    new TraverseFilter[Set] {
      val traverse: Traverse[Set] = setTraverse

      def traverseFilter[G[_], A, B](fa: Set[A])(f: A => G[Option[B]])(implicit G: Applicative[G]): G[Set[B]] =
        fa.foldLeft(G.pure(Set.empty[B])) { (gSet, a) =>
          G.map2(f(a), gSet) {
            case (Some(b), set) => set + b
            case (None, set)    => set
          }
        }
    }
}

@reexports(SetInstances)
object set extends LegacySetInstances with LegacySetInstancesBinCompat0

// TODO: remove when cats.{ Set, Traverse } support export-hook
trait LegacySetInstances {
  implicit def legacySetMonad(implicit e: ExportOrphan[Monad[Set]]): Monad[Set] = e.instance

  implicit def legacySetTraverse(implicit e: ExportOrphan[Traverse[Set]]): Traverse[Set] = e.instance
}

trait LegacySetInstancesBinCompat0 {
  implicit def legacySetTraverseFilter(implicit e: ExportOrphan[TraverseFilter[Set]]): TraverseFilter[Set] = e.instance
}
