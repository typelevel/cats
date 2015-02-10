package cats
package std

import algebra.Eq

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

trait ListInstances {
  implicit val listInstance: Traverse[List] with MonadCombine[List] with CoFlatMap[List] =
    new Traverse[List] with MonadCombine[List] with CoFlatMap[List] {

      def empty[A]: List[A] = Nil

      def combine[A](x: List[A], y: List[A]): List[A] = x ++ y

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

      def foldRight[A, B](fa: List[A], b: B)(f: (A, B) => B): B =
        fa.foldRight(b)(f)

      def foldRight[A, B](fa: List[A], b: Lazy[B])(f: (A, Lazy[B]) => B): Lazy[B] = {
        // we use Lazy.byName(...) to avoid memoizing intermediate values.
        def loop(as: List[A], b: Lazy[B]): Lazy[B] =
          as match {
            case Nil => b
            case a :: rest => Lazy.byName(f(a, foldRight(rest, b)(f)))
          }
        // we memoize the first "step" with Lazy(...).
        Lazy(loop(fa, b).force)
      }

      def traverse[G[_]: Applicative, A, B](fa: List[A])(f: A => G[B]): G[List[B]] = {
        val G = Applicative[G]
        val init = G.pure(ListBuffer.empty[B])
        val gbuf = fa.foldLeft(init) { (gbuf, a) =>
          G.map2(f(a), gbuf)((b, buf) => buf += b)
        }
        G.map(gbuf)(_.toList)
      }
    }

  // TODO: eventually use algebra's instances (which will deal with
  // implicit priority between Eq/PartialOrder/Order).

  implicit def eqList[A](implicit ev: Eq[A]): Eq[List[A]] =
    new Eq[List[A]] {
      def eqv(x: List[A], y: List[A]): Boolean = {
        def loop(xs: List[A], ys: List[A]): Boolean =
          xs match {
            case Nil => ys.isEmpty
            case a :: xs =>
              ys match {
                case Nil => false
                case b :: ys => if (ev.neqv(a, b)) false else loop(xs, ys)
              }
          }
        loop(x, y)
      }
    }
}
