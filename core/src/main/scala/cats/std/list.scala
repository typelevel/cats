package cats
package std

import algebra.Eq
import algebra.std.{ListMonoid, ListOrder}

import cats.data.Streaming
import cats.syntax.order._
import cats.syntax.show._

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

trait ListInstances extends ListInstances1 {
  implicit val listInstance: Traverse[List] with MonadCombine[List] with CoflatMap[List] =
    new Traverse[List] with MonadCombine[List] with CoflatMap[List] {

      def empty[A]: List[A] = Nil

      def combineK[A](x: List[A], y: List[A]): List[A] = x ++ y

      def pure[A](x: A): List[A] = x :: Nil

      override def map[A, B](fa: List[A])(f: A => B): List[B] =
        fa.map(f)

      def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] =
        fa.flatMap(f)

      override def map2[A, B, Z](fa: List[A], fb: List[B])(f: (A, B) => Z): List[Z] =
        fa.flatMap(a => fb.map(b => f(a, b)))

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

      def traverse[G[_], A, B](fa: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] = {
        val gba = G.pure(Vector.empty[B])
        val gbb = fa.foldLeft(gba)((buf, a) => G.map2(buf, f(a))(_ :+ _))
        G.map(gbb)(_.toList)
      }

      override def exists[A](fa: List[A])(p: A => Boolean): Boolean =
        fa.exists(p)

      override def forall[A](fa: List[A])(p: A => Boolean): Boolean =
        fa.forall(p)

      override def isEmpty[A](fa: List[A]): Boolean = fa.isEmpty

      override def toStreaming[A](fa: List[A]): Streaming[A] =
        Streaming.fromList(fa)
    }

  implicit def listAlgebra[A]: Monoid[List[A]] = new ListMonoid[A]
  implicit def listOrder[A: Order]: Order[List[A]] = new ListOrder[A]

  implicit def listShow[A:Show]: Show[List[A]] =
    new Show[List[A]] {
      def show(fa: List[A]): String = fa.map(_.show).mkString("List(", ", ", ")")
    }
}

private[std] sealed trait ListInstances1 extends ListInstances2 {
  implicit def partialOrderList[A: PartialOrder]: PartialOrder[List[A]] =
    new PartialOrder[List[A]] {
      def partialCompare(x: List[A], y: List[A]): Double = {
        def loop(xs: List[A], ys: List[A]): Double =
          xs match {
            case a :: xs =>
              ys match {
                case b :: ys =>
                  val n = a partialCompare b
                  if (n != 0.0) n else loop(xs, ys)
                case Nil =>
                  1.0
              }
            case Nil =>
              if (ys.isEmpty) 0.0 else -1.0
          }
        loop(x, y)
      }
    }
}

private[std] sealed trait ListInstances2 {
  implicit def eqList[A: Eq]: Eq[List[A]] =
    new Eq[List[A]] {
      def eqv(x: List[A], y: List[A]): Boolean = {
        def loop(xs: List[A], ys: List[A]): Boolean =
          xs match {
            case a :: xs =>
              ys match {
                case b :: ys =>
                  if (a =!= b) false else loop(xs, ys)
                case Nil =>
                  false
              }
            case Nil =>
              ys.isEmpty
          }
        loop(x, y)
      }
    }
}
