package cats
package instances

import cats.kernel
import cats.syntax.show._
import cats.data.Ior
import cats.data.ZipLazyList

import scala.annotation.tailrec

trait LazyListInstances extends cats.kernel.instances.LazyListInstances {

  implicit val catsStdInstancesForLazyList
    : Traverse[LazyList] with Alternative[LazyList] with Monad[LazyList] with CoflatMap[LazyList] with Align[LazyList] =
    new Traverse[LazyList]
      with Alternative[LazyList]
      with Monad[LazyList]
      with CoflatMap[LazyList]
      with Align[LazyList] {

      def empty[A]: LazyList[A] = LazyList.empty

      def combineK[A](x: LazyList[A], y: LazyList[A]): LazyList[A] = x.lazyAppendedAll(y)

      def pure[A](x: A): LazyList[A] = LazyList(x)

      override def map[A, B](fa: LazyList[A])(f: A => B): LazyList[B] =
        fa.map(f)

      def flatMap[A, B](fa: LazyList[A])(f: A => LazyList[B]): LazyList[B] =
        fa.flatMap(f)

      override def map2[A, B, Z](fa: LazyList[A], fb: LazyList[B])(f: (A, B) => Z): LazyList[Z] =
        if (fb.isEmpty) LazyList.empty // do O(1) work if fb is empty
        else fa.flatMap(a => fb.map(b => f(a, b))) // already O(1) if fa is empty

      override def map2Eval[A, B, Z](fa: LazyList[A], fb: Eval[LazyList[B]])(f: (A, B) => Z): Eval[LazyList[Z]] =
        if (fa.isEmpty) Eval.now(LazyList.empty) // no need to evaluate fb
        else fb.map(fb => map2(fa, fb)(f))

      def coflatMap[A, B](fa: LazyList[A])(f: LazyList[A] => B): LazyList[B] =
        fa.tails.to(LazyList).init.map(f)

      def foldLeft[A, B](fa: LazyList[A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b)(f)

      def foldRight[A, B](fa: LazyList[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        Now(fa).flatMap { s =>
          // Note that we don't use pattern matching to deconstruct the
          // stream, since that would needlessly force the tail.
          if (s.isEmpty) lb else f(s.head, Eval.defer(foldRight(s.tail, lb)(f)))
        }

      override def foldMap[A, B](fa: LazyList[A])(f: A => B)(implicit B: Monoid[B]): B =
        B.combineAll(fa.iterator.map(f))

      def traverse[G[_], A, B](fa: LazyList[A])(f: A => G[B])(implicit G: Applicative[G]): G[LazyList[B]] =
        // We use foldRight to avoid possible stack overflows. Since
        // we don't want to return a Eval[_] instance, we call .value
        // at the end.
        foldRight(fa, Always(G.pure(LazyList.empty[B])))((a, lgsb) => G.map2Eval(f(a), lgsb)(_ #:: _)).value

      override def mapWithIndex[A, B](fa: LazyList[A])(f: (A, Int) => B): LazyList[B] =
        fa.zipWithIndex.map(ai => f(ai._1, ai._2))

      override def zipWithIndex[A](fa: LazyList[A]): LazyList[(A, Int)] =
        fa.zipWithIndex

      def tailRecM[A, B](a: A)(fn: A => LazyList[Either[A, B]]): LazyList[B] = {
        val kernel = Iterator.unfold[Option[B], Iterator[Either[A, B]]](Iterator(Left(a))) { it =>
          if (!it.hasNext) None
          else
            it.next match {
              case Left(a)  => Some((None, fn(a).iterator ++ it))
              case Right(b) => Some((Some(b), it))
            }
        }
        LazyList.from(kernel.collect { case Some(v) => v })
      }

      override def exists[A](fa: LazyList[A])(p: A => Boolean): Boolean =
        fa.exists(p)

      override def forall[A](fa: LazyList[A])(p: A => Boolean): Boolean =
        fa.forall(p)

      override def get[A](fa: LazyList[A])(idx: Long): Option[A] = {
        @tailrec
        def go(idx: Long, s: LazyList[A]): Option[A] =
          s match {
            case h #:: tail =>
              if (idx == 0L) Some(h) else go(idx - 1L, tail)
            case _ => None
          }
        if (idx < 0L) None else go(idx, fa)
      }

      override def isEmpty[A](fa: LazyList[A]): Boolean = fa.isEmpty

      override def foldM[G[_], A, B](fa: LazyList[A], z: B)(f: (B, A) => G[B])(implicit G: Monad[G]): G[B] = {
        def step(in: (LazyList[A], B)): G[Either[(LazyList[A], B), B]] = {
          val (s, b) = in
          if (s.isEmpty)
            G.pure(Right(b))
          else {
            G.map(f(b, s.head))(bnext => Left((s.tail, bnext)))
          }
        }

        G.tailRecM((fa, z))(step)
      }

      override def fold[A](fa: LazyList[A])(implicit A: Monoid[A]): A = A.combineAll(fa)

      override def toList[A](fa: LazyList[A]): List[A] = fa.toList

      override def toIterable[A](fa: LazyList[A]): Iterable[A] = fa

      override def reduceLeftOption[A](fa: LazyList[A])(f: (A, A) => A): Option[A] =
        fa.reduceLeftOption(f)

      override def find[A](fa: LazyList[A])(f: A => Boolean): Option[A] = fa.find(f)

      override def algebra[A]: Monoid[LazyList[A]] = new kernel.instances.LazyListMonoid[A]

      override def collectFirst[A, B](fa: LazyList[A])(pf: PartialFunction[A, B]): Option[B] = fa.collectFirst(pf)

      override def collectFirstSome[A, B](fa: LazyList[A])(f: A => Option[B]): Option[B] =
        fa.collectFirst(Function.unlift(f))

      def functor: Functor[LazyList] = this

      def align[A, B](fa: LazyList[A], fb: LazyList[B]): LazyList[Ior[A, B]] =
        alignWith(fa, fb)(identity)

      override def alignWith[A, B, C](fa: LazyList[A], fb: LazyList[B])(f: Ior[A, B] => C): LazyList[C] =
        LazyList.from(Align.alignWithIterator[A, B, C](fa, fb)(f))
    }

  implicit def catsStdShowForLazyList[A: Show]: Show[LazyList[A]] =
    new Show[LazyList[A]] {
      def show(fa: LazyList[A]): String = if (fa.isEmpty) "LazyList()" else s"LazyList(${fa.head.show}, ?)"
    }

  implicit val catsStdTraverseFilterForLazyList: TraverseFilter[LazyList] = new TraverseFilter[LazyList] {
    val traverse: Traverse[LazyList] = catsStdInstancesForLazyList

    override def mapFilter[A, B](fa: LazyList[A])(f: (A) => Option[B]): LazyList[B] =
      fa.collect(Function.unlift(f))

    override def filter[A](fa: LazyList[A])(f: (A) => Boolean): LazyList[A] = fa.filter(f)

    override def filterNot[A](fa: LazyList[A])(f: (A) => Boolean): LazyList[A] = fa.filterNot(f)

    override def collect[A, B](fa: LazyList[A])(f: PartialFunction[A, B]): LazyList[B] = fa.collect(f)

    override def flattenOption[A](fa: LazyList[Option[A]]): LazyList[A] = fa.flatten

    def traverseFilter[G[_], A, B](
      fa: LazyList[A]
    )(f: (A) => G[Option[B]])(implicit G: Applicative[G]): G[LazyList[B]] =
      fa.foldRight(Eval.now(G.pure(LazyList.empty[B])))((x, xse) => G.map2Eval(f(x), xse)((i, o) => i.fold(o)(_ +: o)))
        .value

    override def filterA[G[_], A](fa: LazyList[A])(f: (A) => G[Boolean])(implicit G: Applicative[G]): G[LazyList[A]] =
      fa.foldRight(Eval.now(G.pure(LazyList.empty[A])))((x, xse) =>
          G.map2Eval(f(x), xse)((b, as) => if (b) x +: as else as)
        )
        .value

  }

  implicit def catsStdParallelForLazyListZipLazyList[A]: Parallel.Aux[LazyList, ZipLazyList] =
    new Parallel[LazyList] {
      type F[x] = ZipLazyList[x]

      def monad: Monad[LazyList] = cats.instances.lazyList.catsStdInstancesForLazyList
      def applicative: Applicative[ZipLazyList] = ZipLazyList.catsDataAlternativeForZipLazyList

      def sequential: ZipLazyList ~> LazyList =
        λ[ZipLazyList ~> LazyList](_.value)

      def parallel: LazyList ~> ZipLazyList =
        λ[LazyList ~> ZipLazyList](v => new ZipLazyList(v))
    }
}
