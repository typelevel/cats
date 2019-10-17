package cats
package instances

import cats.data.ZipList
import cats.syntax.show._

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

trait ListInstances extends cats.kernel.instances.ListInstances {

  implicit val catsStdInstancesForList: Traverse[List] with Alternative[List] with Monad[List] with CoflatMap[List] =
    new Traverse[List] with Alternative[List] with Monad[List] with CoflatMap[List] {
      def empty[A]: List[A] = Nil

      def combineK[A](x: List[A], y: List[A]): List[A] = x ++ y

      def pure[A](x: A): List[A] = x :: Nil

      override def map[A, B](fa: List[A])(f: A => B): List[B] =
        fa.map(f)

      def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] =
        fa.flatMap(f)

      override def map2[A, B, Z](fa: List[A], fb: List[B])(f: (A, B) => Z): List[Z] =
        if (fb.isEmpty) Nil // do O(1) work if fb is empty
        else fa.flatMap(a => fb.map(b => f(a, b))) // already O(1) if fa is empty

      override def map2Eval[A, B, Z](fa: List[A], fb: Eval[List[B]])(f: (A, B) => Z): Eval[List[Z]] =
        if (fa.isEmpty) Eval.now(Nil) // no need to evaluate fb
        else fb.map(fb => map2(fa, fb)(f))

      def tailRecM[A, B](a: A)(f: A => List[Either[A, B]]): List[B] = {
        val buf = List.newBuilder[B]
        @tailrec def go(lists: List[List[Either[A, B]]]): Unit = lists match {
          case (ab :: abs) :: tail =>
            ab match {
              case Right(b) => buf += b; go(abs :: tail)
              case Left(a)  => go(f(a) :: abs :: tail)
            }
          case Nil :: tail => go(tail)
          case Nil         => ()
        }
        go(f(a) :: Nil)
        buf.result
      }

      def coflatMap[A, B](fa: List[A])(f: List[A] => B): List[B] = {
        @tailrec def loop(buf: ListBuffer[B], as: List[A]): List[B] =
          as match {
            case Nil       => buf.toList
            case _ :: rest => loop(buf += f(as), rest)
          }
        loop(ListBuffer.empty[B], fa)
      }

      def foldLeft[A, B](fa: List[A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b)(f)

      def foldRight[A, B](fa: List[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
        def loop(as: List[A]): Eval[B] =
          as match {
            case Nil    => lb
            case h :: t => f(h, Eval.defer(loop(t)))
          }
        Eval.defer(loop(fa))
      }

      override def foldMap[A, B](fa: List[A])(f: A => B)(implicit B: Monoid[B]): B =
        B.combineAll(fa.iterator.map(f))

      def traverse[G[_], A, B](fa: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] =
        foldRight[A, G[List[B]]](fa, Always(G.pure(List.empty))) { (a, lglb) =>
          G.map2Eval(f(a), lglb)(_ :: _)
        }.value

      override def mapWithIndex[A, B](fa: List[A])(f: (A, Int) => B): List[B] =
        fa.iterator.zipWithIndex.map(ai => f(ai._1, ai._2)).toList

      override def zipWithIndex[A](fa: List[A]): List[(A, Int)] =
        fa.zipWithIndex

      override def partitionEither[A, B, C](
        fa: List[A]
      )(f: (A) => Either[B, C])(implicit A: Alternative[List]): (List[B], List[C]) =
        fa.foldRight((List.empty[B], List.empty[C]))(
          (a, acc) =>
            f(a) match {
              case Left(b)  => (b :: acc._1, acc._2)
              case Right(c) => (acc._1, c :: acc._2)
            }
        )

      @tailrec
      override def get[A](fa: List[A])(idx: Long): Option[A] =
        fa match {
          case Nil => None
          case h :: tail =>
            if (idx < 0) None
            else if (idx == 0) Some(h)
            else get(tail)(idx - 1)
        }

      override def exists[A](fa: List[A])(p: A => Boolean): Boolean =
        fa.exists(p)

      override def forall[A](fa: List[A])(p: A => Boolean): Boolean =
        fa.forall(p)

      override def isEmpty[A](fa: List[A]): Boolean = fa.isEmpty

      override def foldM[G[_], A, B](fa: List[A], z: B)(f: (B, A) => G[B])(implicit G: Monad[G]): G[B] = {
        def step(in: (List[A], B)): G[Either[(List[A], B), B]] = in match {
          case (Nil, b) => G.pure(Right(b))
          case (a :: tail, b) =>
            G.map(f(b, a)) { bnext =>
              Left((tail, bnext))
            }
        }

        G.tailRecM((fa, z))(step)
      }

      override def fold[A](fa: List[A])(implicit A: Monoid[A]): A = A.combineAll(fa)

      override def toList[A](fa: List[A]): List[A] = fa

      override def reduceLeftOption[A](fa: List[A])(f: (A, A) => A): Option[A] =
        fa.reduceLeftOption(f)

      override def find[A](fa: List[A])(f: A => Boolean): Option[A] = fa.find(f)

      override def filter_[A](fa: List[A])(p: A => Boolean): List[A] = fa.filter(p)

      override def takeWhile_[A](fa: List[A])(p: A => Boolean): List[A] = fa.takeWhile(p)

      override def dropWhile_[A](fa: List[A])(p: A => Boolean): List[A] = fa.dropWhile(p)

      override def algebra[A]: Monoid[List[A]] = new kernel.instances.ListMonoid[A]

      override def collectFirst[A, B](fa: List[A])(pf: PartialFunction[A, B]): Option[B] = fa.collectFirst(pf)

      override def collectFirstSome[A, B](fa: List[A])(f: A => Option[B]): Option[B] =
        fa.collectFirst(Function.unlift(f))

    }

  implicit def catsStdShowForList[A: Show]: Show[List[A]] =
    new Show[List[A]] {
      def show(fa: List[A]): String =
        fa.iterator.map(_.show).mkString("List(", ", ", ")")
    }

  implicit def catsStdNonEmptyParallelForListZipList: NonEmptyParallel.Aux[List, ZipList] =
    new NonEmptyParallel[List] {
      type F[x] = ZipList[x]

      def flatMap: FlatMap[List] = cats.instances.list.catsStdInstancesForList
      def apply: Apply[ZipList] = ZipList.catsDataCommutativeApplyForZipList

      def sequential: ZipList ~> List =
        λ[ZipList ~> List](_.value)

      def parallel: List ~> ZipList =
        λ[List ~> ZipList](v => new ZipList(v))
    }
}

private[instances] trait ListInstancesBinCompat0 {
  implicit val catsStdTraverseFilterForList: TraverseFilter[List] = new TraverseFilter[List] {
    val traverse: Traverse[List] = cats.instances.list.catsStdInstancesForList

    override def mapFilter[A, B](fa: List[A])(f: (A) => Option[B]): List[B] = fa.collect(Function.unlift(f))

    override def filter[A](fa: List[A])(f: (A) => Boolean): List[A] = fa.filter(f)

    override def collect[A, B](fa: List[A])(f: PartialFunction[A, B]): List[B] = fa.collect(f)

    override def flattenOption[A](fa: List[Option[A]]): List[A] = fa.flatten

    def traverseFilter[G[_], A, B](fa: List[A])(f: (A) => G[Option[B]])(implicit G: Applicative[G]): G[List[B]] =
      fa.foldRight(Eval.now(G.pure(List.empty[B])))(
          (x, xse) => G.map2Eval(f(x), xse)((i, o) => i.fold(o)(_ :: o))
        )
        .value

    override def filterA[G[_], A](fa: List[A])(f: (A) => G[Boolean])(implicit G: Applicative[G]): G[List[A]] =
      fa.foldRight(Eval.now(G.pure(List.empty[A])))(
          (x, xse) => G.map2Eval(f(x), xse)((b, list) => if (b) x :: list else list)
        )
        .value
  }
}
