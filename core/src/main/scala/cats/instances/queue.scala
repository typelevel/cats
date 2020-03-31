package cats
package instances

import cats.syntax.show._
import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.util.Try

trait QueueInstances extends cats.kernel.instances.QueueInstances {

  implicit val catsStdInstancesForQueue
    : Traverse[Queue] with Alternative[Queue] with Monad[Queue] with CoflatMap[Queue] =
    new Traverse[Queue] with Alternative[Queue] with Monad[Queue] with CoflatMap[Queue] {
      def empty[A]: Queue[A] = Queue.empty

      def combineK[A](x: Queue[A], y: Queue[A]): Queue[A] = x ++ y

      def pure[A](x: A): Queue[A] = Queue(x)

      override def map[A, B](fa: Queue[A])(f: A => B): Queue[B] =
        fa.map(f)

      def flatMap[A, B](fa: Queue[A])(f: A => Queue[B]): Queue[B] =
        fa.flatMap(f)

      override def map2[A, B, Z](fa: Queue[A], fb: Queue[B])(f: (A, B) => Z): Queue[Z] =
        if (fb.isEmpty) Queue.empty // do O(1) work if fb is empty
        else fa.flatMap(a => fb.map(b => f(a, b))) // already O(1) if fa is empty

      override def map2Eval[A, B, Z](fa: Queue[A], fb: Eval[Queue[B]])(f: (A, B) => Z): Eval[Queue[Z]] =
        if (fa.isEmpty) Eval.now(Queue.empty) // no need to evaluate fb
        else fb.map(fb => map2(fa, fb)(f))

      def tailRecM[A, B](a: A)(f: A => Queue[Either[A, B]]): Queue[B] = {
        val bldr = Queue.newBuilder[B]
        @tailrec def go(lists: List[Queue[Either[A, B]]]): Queue[B] =
          lists match {
            case q :: tail =>
              if (q.isEmpty) go(tail)
              else {
                val (e, es) = q.dequeue
                e match {
                  case Right(b) => bldr += b; go(es :: tail)
                  case Left(a)  => go(f(a) :: es :: tail)
                }
              }
            case Nil =>
              bldr.result
          }
        go(f(a) :: Nil)
      }

      def coflatMap[A, B](fa: Queue[A])(f: Queue[A] => B): Queue[B] = {
        val bldr = Queue.newBuilder[B]
        @tailrec def loop(as: Queue[A]): Queue[B] =
          if (as.isEmpty) bldr.result
          else {
            val (_, rest) = as.dequeue
            bldr += f(as)
            loop(rest)
          }
        loop(fa)
      }

      def foldLeft[A, B](fa: Queue[A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b)(f)

      def foldRight[A, B](fa: Queue[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
        def loop(as: Queue[A]): Eval[B] =
          if (as.isEmpty) lb
          else {
            val (h, t) = as.dequeue
            f(h, Eval.defer(loop(t)))
          }
        Eval.defer(loop(fa))
      }

      override def foldMap[A, B](fa: Queue[A])(f: A => B)(implicit B: Monoid[B]): B =
        B.combineAll(fa.iterator.map(f))

      def traverse[G[_], A, B](fa: Queue[A])(f: A => G[B])(implicit G: Applicative[G]): G[Queue[B]] =
        foldRight[A, G[Queue[B]]](fa, Always(G.pure(Queue.empty))) { (a, lglb) =>
          G.map2Eval(f(a), lglb)(_ +: _)
        }.value

      override def mapWithIndex[A, B](fa: Queue[A])(f: (A, Int) => B): Queue[B] = {
        val b = Queue.newBuilder[B]
        fa.iterator.zipWithIndex.map(ai => f(ai._1, ai._2)).foreach(b += _)
        b.result
      }

      override def zipWithIndex[A](fa: Queue[A]): Queue[(A, Int)] =
        fa.zipWithIndex

      override def get[A](fa: Queue[A])(idx: Long): Option[A] =
        if (idx < 0 || idx > Int.MaxValue) None
        else Try(fa(idx.toInt)).toOption

      override def exists[A](fa: Queue[A])(p: A => Boolean): Boolean =
        fa.exists(p)

      override def forall[A](fa: Queue[A])(p: A => Boolean): Boolean =
        fa.forall(p)

      override def isEmpty[A](fa: Queue[A]): Boolean = fa.isEmpty

      override def foldM[G[_], A, B](fa: Queue[A], z: B)(f: (B, A) => G[B])(implicit G: Monad[G]): G[B] = {
        def step(in: (Queue[A], B)): G[Either[(Queue[A], B), B]] = {
          val (xs, b) = in
          if (xs.isEmpty) G.pure(Right(b))
          else {
            val (a, tail) = xs.dequeue
            G.map(f(b, a)) { bnext =>
              Left((tail, bnext))
            }
          }
        }

        G.tailRecM((fa, z))(step)
      }

      override def fold[A](fa: Queue[A])(implicit A: Monoid[A]): A = A.combineAll(fa)

      override def toList[A](fa: Queue[A]): List[A] = fa.toList

      override def reduceLeftOption[A](fa: Queue[A])(f: (A, A) => A): Option[A] =
        fa.reduceLeftOption(f)

      override def find[A](fa: Queue[A])(f: A => Boolean): Option[A] =
        fa.find(f)

      override def filter_[A](fa: Queue[A])(p: A => Boolean): List[A] =
        fa.iterator.filter(p).toList

      override def takeWhile_[A](fa: Queue[A])(p: A => Boolean): List[A] =
        fa.iterator.takeWhile(p).toList

      override def dropWhile_[A](fa: Queue[A])(p: A => Boolean): List[A] =
        fa.iterator.dropWhile(p).toList

      override def algebra[A]: Monoid[Queue[A]] =
        new kernel.instances.QueueMonoid[A]

      override def collectFirst[A, B](fa: Queue[A])(pf: PartialFunction[A, B]): Option[B] = fa.collectFirst(pf)

      override def collectFirstSome[A, B](fa: Queue[A])(f: A => Option[B]): Option[B] =
        fa.collectFirst(Function.unlift(f))
    }

  implicit def catsStdShowForQueue[A: Show]: Show[Queue[A]] =
    new Show[Queue[A]] {
      def show(fa: Queue[A]): String =
        fa.iterator.map(_.show).mkString("Queue(", ", ", ")")
    }

  implicit def catsStdTraverseFilterForQueue: TraverseFilter[Queue] = QueueInstances.catsStdTraverseFilterForQueue
}

private object QueueInstances {
  private val catsStdTraverseFilterForQueue: TraverseFilter[Queue] = new TraverseFilter[Queue] {
    val traverse: Traverse[Queue] = cats.instances.queue.catsStdInstancesForQueue

    override def mapFilter[A, B](fa: Queue[A])(f: (A) => Option[B]): Queue[B] =
      fa.collect(Function.unlift(f))

    override def filter[A](fa: Queue[A])(f: (A) => Boolean): Queue[A] = fa.filter(f)

    override def filterNot[A](fa: Queue[A])(f: A => Boolean): Queue[A] = fa.filterNot(f)

    override def collect[A, B](fa: Queue[A])(f: PartialFunction[A, B]): Queue[B] = fa.collect(f)

    override def flattenOption[A](fa: Queue[Option[A]]): Queue[A] = fa.flatten

    def traverseFilter[G[_], A, B](fa: Queue[A])(f: (A) => G[Option[B]])(implicit G: Applicative[G]): G[Queue[B]] =
      traverse
        .foldRight(fa, Eval.now(G.pure(Queue.empty[B])))((x, xse) => G.map2Eval(f(x), xse)((i, o) => i.fold(o)(_ +: o)))
        .value

    override def filterA[G[_], A](fa: Queue[A])(f: (A) => G[Boolean])(implicit G: Applicative[G]): G[Queue[A]] =
      traverse
        .foldRight(fa, Eval.now(G.pure(Queue.empty[A])))((x, xse) =>
          G.map2Eval(f(x), xse)((b, queue) => if (b) x +: queue else queue)
        )
        .value
  }
}
