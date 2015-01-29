package cats
package std

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

trait ListInstances {
  implicit val listInstance =
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
        val gba = G.pure(ListBuffer.empty[B])
        val gbb = fa.foldLeft(gba)((buf, a) => G.map2(buf, f(a))(_ += _))
        G.map(gbb)(_.toList)
      }
    }
}
