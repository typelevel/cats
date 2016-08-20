package cats
package instances

import cats.syntax.show._

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

trait ListInstances extends cats.kernel.instances.ListInstances {

  implicit val catsStdInstancesForList: TraverseFilter[List] with MonadCombine[List] with Monad[List] with CoflatMap[List] with RecursiveTailRecM[List] =
    new TraverseFilter[List] with MonadCombine[List] with Monad[List] with CoflatMap[List] with RecursiveTailRecM[List] {

      def empty[A]: List[A] = Nil

      def combineK[A](x: List[A], y: List[A]): List[A] = x ++ y

      def pure[A](x: A): List[A] = x :: Nil

      override def map[A, B](fa: List[A])(f: A => B): List[B] =
        fa.map(f)

      def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] =
        fa.flatMap(f)

      override def map2[A, B, Z](fa: List[A], fb: List[B])(f: (A, B) => Z): List[Z] =
        fa.flatMap(a => fb.map(b => f(a, b)))

      def tailRecM[A, B](a: A)(f: A => List[Either[A, B]]): List[B] = {
        val buf = List.newBuilder[B]
        @tailrec def go(lists: List[List[Either[A, B]]]): Unit = lists match {
          case (ab :: abs) :: tail => ab match {
            case Right(b) => buf += b; go(abs :: tail)
            case Left(a) => go(f(a) :: abs :: tail)
          }
          case Nil :: tail => go(tail)
          case Nil => ()
        }
        go(f(a) :: Nil)
        buf.result
      }

      def coflatMap[A, B](fa: List[A])(f: List[A] => B): List[B] = {
        @tailrec def loop(buf: ListBuffer[B], as: List[A]): List[B] =
          as match {
            case Nil => buf.toList
            case _ :: rest => loop(buf += f(as), rest)
          }
        loop(ListBuffer.empty[B], fa)
      }

      def foldLeft[A, B](fa: List[A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b)(f)

      def foldRight[A, B](fa: List[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
        def loop(as: List[A]): Eval[B] =
          as match {
            case Nil => lb
            case h :: t => f(h, Eval.defer(loop(t)))
          }
        Eval.defer(loop(fa))
      }

      def traverseFilter[G[_], A, B](fa: List[A])(f: A => G[Option[B]])(implicit G: Applicative[G]): G[List[B]] =
        foldRight[A, G[List[B]]](fa, Always(G.pure(List.empty))){ (a, lglb) =>
          G.map2Eval(f(a), lglb)((ob, l) => ob.fold(l)(_ :: l))
        }.value

      override def traverse[G[_], A, B](fa: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] =
        foldRight[A, G[List[B]]](fa, Always(G.pure(List.empty))){ (a, lglb) =>
          G.map2Eval(f(a), lglb)(_ :: _)
        }.value

      override def exists[A](fa: List[A])(p: A => Boolean): Boolean =
        fa.exists(p)

      override def forall[A](fa: List[A])(p: A => Boolean): Boolean =
        fa.forall(p)

      override def isEmpty[A](fa: List[A]): Boolean = fa.isEmpty

      override def filter[A](fa: List[A])(f: A => Boolean): List[A] = fa.filter(f)
    }

  implicit def catsStdShowForList[A:Show]: Show[List[A]] =
    new Show[List[A]] {
      def show(fa: List[A]): String = fa.map(_.show).mkString("List(", ", ", ")")
    }
}
