package cats
package std

import scala.annotation.tailrec
import scala.collection.immutable.VectorBuilder

trait VectorInstances {
  implicit val vectorInstance: Traverse[Vector] with MonadCombine[Vector] =
    new Traverse[Vector] with MonadCombine[Vector] {

      def empty[A]: Vector[A] = Vector.empty[A]

      def combine[A](x: Vector[A], y: Vector[A]): Vector[A] = x ++ y

      def pure[A](x: A): Vector[A] = Vector(x)

      override def map[A, B](fa: Vector[A])(f: A => B): Vector[B] =
        fa.map(f)

      def flatMap[A, B](fa: Vector[A])(f: A => Vector[B]): Vector[B] =
        fa.flatMap(f)

      override def map2[A, B, Z](fa: Vector[A], fb: Vector[B])(f: (A, B) => Z): Vector[Z] =
        fa.flatMap(a => fb.map(b => f(a, b)))

      def foldLeft[A, B](fa: Vector[A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b)(f)

      def foldRight[A, B](fa: Vector[A], b: B)(f: (A, B) => B): B =
        fa.foldRight(b)(f)

      def foldRight[A, B](fa: Vector[A], b: Lazy[B])(f: (A, Lazy[B]) => B): Lazy[B] = {
        val it = fa.iterator
        def loop(b: Lazy[B]): Lazy[B] =
          if (it.hasNext) Lazy.byName(f(it.next, b)) else b
        Lazy(loop(b).value)
      }

      def traverse[G[_]: Applicative, A, B](fa: Vector[A])(f: A => G[B]): G[Vector[B]] = {
        val G = Applicative[G]
        val gba = G.pure(new VectorBuilder[B])
        val gbb = fa.foldLeft(gba)((buf, a) => G.map2(buf, f(a))(_ += _))
        G.map(gbb)(_.result)
      }
    }
}
