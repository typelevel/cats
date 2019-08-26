package cats
package instances

import cats.syntax.show._
import scala.collection.immutable.Seq
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.language.higherKinds

trait SeqInstances extends cats.kernel.instances.SeqInstances {

  implicit val catsStdInstancesForSeq: Traverse[Seq] with Alternative[Seq] with Monad[Seq] with CoflatMap[Seq] =
    new Traverse[Seq] with Alternative[Seq] with Monad[Seq] with CoflatMap[Seq] {
      def empty[A]: Seq[A] = Seq.empty

      def combineK[A](x: Seq[A], y: Seq[A]): Seq[A] = x ++ y

      def pure[A](x: A): Seq[A] = x +: Seq.empty

      override def map[A, B](fa: Seq[A])(f: A => B): Seq[B] =
        fa.map(f)

      def flatMap[A, B](fa: Seq[A])(f: A => Seq[B]): Seq[B] =
        fa.flatMap(f)

      override def map2[A, B, Z](fa: Seq[A], fb: Seq[B])(f: (A, B) => Z): Seq[Z] =
        if (fb.isEmpty) Seq.empty // do O(1) work if fb is empty
        else fa.flatMap(a => fb.map(b => f(a, b))) // already O(1) if fa is empty

      override def map2Eval[A, B, Z](fa: Seq[A], fb: Eval[Seq[B]])(f: (A, B) => Z): Eval[Seq[Z]] =
        if (fa.isEmpty) Eval.now(Seq.empty) // no need to evaluate fb
        else fb.map(fb => map2(fa, fb)(f))

      def tailRecM[A, B](a: A)(f: A => Seq[Either[A, B]]): Seq[B] = {
        val buf = Seq.newBuilder[B]
        @tailrec def go(lists: Seq[Seq[Either[A, B]]]): Unit = lists match {
          case (ab +: abs) +: tail =>
            ab match {
              case Right(b) => buf += b; go(abs +: tail)
              case Left(a1) => go(f(a1) +: abs +: tail)
            }
          case Seq() +: tail => go(tail)
          case Seq()         => ()
        }
        go(f(a) +: Seq.empty)
        buf.result
      }

      def coflatMap[A, B](fa: Seq[A])(f: Seq[A] => B): Seq[B] = {
        @tailrec def loop(buf: ListBuffer[B], as: Seq[A]): Seq[B] =
          as match {
            case Seq()     => buf.toList
            case _ +: rest => loop(buf += f(as), rest)
          }
        loop(ListBuffer.empty[B], fa)
      }

      def foldLeft[A, B](fa: Seq[A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b)(f)

      def foldRight[A, B](fa: Seq[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
        def loop(as: Seq[A]): Eval[B] =
          as match {
            case Seq()  => lb
            case h +: t => f(h, Eval.defer(loop(t)))
          }
        Eval.defer(loop(fa))
      }

      override def foldMap[A, B](fa: Seq[A])(f: A => B)(implicit B: Monoid[B]): B =
        B.combineAll(fa.iterator.map(f))

      def traverse[G[_], A, B](fa: Seq[A])(f: A => G[B])(implicit G: Applicative[G]): G[Seq[B]] =
        foldRight[A, G[Seq[B]]](fa, Always(G.pure(Seq.empty))) { (a, lglb) =>
          G.map2Eval(f(a), lglb)(_ +: _)
        }.value

      override def mapWithIndex[A, B](fa: Seq[A])(f: (A, Int) => B): Seq[B] =
        fa.iterator.zipWithIndex.map(ai => f(ai._1, ai._2)).toList

      override def zipWithIndex[A](fa: Seq[A]): Seq[(A, Int)] =
        fa.zipWithIndex

      override def partitionEither[A, B, C](
        fa: Seq[A]
      )(f: A => Either[B, C])(implicit A: Alternative[Seq]): (Seq[B], Seq[C]) =
        fa.foldRight((Seq.empty[B], Seq.empty[C]))(
          (a, acc) =>
            f(a) match {
              case Left(b)  => (b +: acc._1, acc._2)
              case Right(c) => (acc._1, c +: acc._2)
            }
        )

      @tailrec
      override def get[A](fa: Seq[A])(idx: Long): Option[A] =
        fa match {
          case Seq() => None
          case h +: tail =>
            if (idx < 0) None
            else if (idx == 0) Some(h)
            else get(tail)(idx - 1)
        }

      override def exists[A](fa: Seq[A])(p: A => Boolean): Boolean =
        fa.exists(p)

      override def forall[A](fa: Seq[A])(p: A => Boolean): Boolean =
        fa.forall(p)

      override def isEmpty[A](fa: Seq[A]): Boolean = fa.isEmpty

      override def foldM[G[_], A, B](fa: Seq[A], z: B)(f: (B, A) => G[B])(implicit G: Monad[G]): G[B] = {
        def step(in: (Seq[A], B)): G[Either[(Seq[A], B), B]] = in match {
          case (Seq(), b) => G.pure(Right(b))
          case (a +: tail, b) =>
            G.map(f(b, a)) { bnext =>
              Left((tail, bnext))
            }
        }

        G.tailRecM((fa, z))(step)
      }

      override def fold[A](fa: Seq[A])(implicit A: Monoid[A]): A = A.combineAll(fa)

      override def toList[A](fa: Seq[A]): List[A] = fa.toList

      override def reduceLeftOption[A](fa: Seq[A])(f: (A, A) => A): Option[A] =
        fa.reduceLeftOption(f)

      override def find[A](fa: Seq[A])(f: A => Boolean): Option[A] = fa.find(f)

      override def filter_[A](fa: Seq[A])(p: A => Boolean): List[A] = fa.filter(p).toList

      override def takeWhile_[A](fa: Seq[A])(p: A => Boolean): List[A] = fa.takeWhile(p).toList

      override def dropWhile_[A](fa: Seq[A])(p: A => Boolean): List[A] = fa.dropWhile(p).toList

      override def algebra[A]: Monoid[Seq[A]] = new kernel.instances.SeqMonoid[A]

      override def collectFirst[A, B](fa: Seq[A])(pf: PartialFunction[A, B]): Option[B] = fa.collectFirst(pf)

      override def collectFirstSome[A, B](fa: Seq[A])(f: A => Option[B]): Option[B] =
        fa.collectFirst(Function.unlift(f))

    }

  implicit def catsStdShowForSeq[A: Show]: Show[Seq[A]] =
    new Show[Seq[A]] {
      def show(fa: Seq[A]): String =
        fa.iterator.map(_.show).mkString("Seq(", ", ", ")")
    }

  implicit val catsStdTraverseFilterForSeq: TraverseFilter[Seq] = new TraverseFilter[Seq] {
    val traverse: Traverse[Seq] = catsStdInstancesForSeq

    override def mapFilter[A, B](fa: Seq[A])(f: A => Option[B]): Seq[B] = fa.collect(Function.unlift(f))

    override def filter[A](fa: Seq[A])(f: A => Boolean): Seq[A] = fa.filter(f)

    override def collect[A, B](fa: Seq[A])(f: PartialFunction[A, B]): Seq[B] = fa.collect(f)

    override def flattenOption[A](fa: Seq[Option[A]]): Seq[A] = fa.flatten

    def traverseFilter[G[_], A, B](fa: Seq[A])(f: A => G[Option[B]])(implicit G: Applicative[G]): G[Seq[B]] =
      fa.foldRight(Eval.now(G.pure(Seq.empty[B])))(
          (x, xse) => G.map2Eval(f(x), xse)((i, o) => i.fold(o)(_ +: o))
        )
        .value

    override def filterA[G[_], A](fa: Seq[A])(f: A => G[Boolean])(implicit G: Applicative[G]): G[Seq[A]] =
      fa.foldRight(Eval.now(G.pure(Seq.empty[A])))(
          (x, xse) => G.map2Eval(f(x), xse)((b, list) => if (b) x +: list else list)
        )
        .value
  }
}
