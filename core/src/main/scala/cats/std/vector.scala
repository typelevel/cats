package cats
package std

import cats.syntax.show._

import scala.annotation.tailrec
import scala.collection.+:
import scala.collection.immutable.VectorBuilder

trait VectorInstances extends cats.kernel.std.VectorInstances {
  implicit val vectorInstance: Traverse[Vector] with MonadCombine[Vector] with CoflatMap[Vector] =
    new Traverse[Vector] with MonadCombine[Vector] with CoflatMap[Vector] {

      def empty[A]: Vector[A] = Vector.empty[A]

      def combineK[A](x: Vector[A], y: Vector[A]): Vector[A] = x ++ y

      def pure[A](x: A): Vector[A] = Vector(x)

      override def map[A, B](fa: Vector[A])(f: A => B): Vector[B] =
        fa.map(f)

      def flatMap[A, B](fa: Vector[A])(f: A => Vector[B]): Vector[B] =
        fa.flatMap(f)

      override def map2[A, B, Z](fa: Vector[A], fb: Vector[B])(f: (A, B) => Z): Vector[Z] =
        fa.flatMap(a => fb.map(b => f(a, b)))

      def coflatMap[A, B](fa: Vector[A])(f: Vector[A] => B): Vector[B] = {
        @tailrec def loop(builder: VectorBuilder[B], as: Vector[A]): Vector[B] =
          as match {
            case _ +: rest => loop(builder += f(as), rest)
            case _ => builder.result()
          }
        loop(new VectorBuilder[B], fa)
      }

      def foldLeft[A, B](fa: Vector[A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b)(f)

      def foldRight[A, B](fa: Vector[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
        def loop(i: Int): Eval[B] =
          if (i < fa.length) f(fa(i), Eval.defer(loop(i + 1))) else lb
        Eval.defer(loop(0))
      }

      def traverse[G[_], A, B](fa: Vector[A])(f: A => G[B])(implicit G: Applicative[G]): G[Vector[B]] =
        fa.foldLeft(G.pure(Vector.empty[B]))((buf, a) => G.map2(buf, f(a))(_ :+ _))

      override def exists[A](fa: Vector[A])(p: A => Boolean): Boolean =
        fa.exists(p)

      override def isEmpty[A](fa: Vector[A]): Boolean = fa.isEmpty
    }

  implicit def vectorShow[A:Show]: Show[Vector[A]] =
    new Show[Vector[A]] {
      def show(fa: Vector[A]): String = fa.map(_.show).mkString("Vector(", ", ", ")")
    }
}
