package cats
package instances

import cats.syntax.show._
import scala.annotation.tailrec
import scala.collection.+:
import scala.collection.immutable.VectorBuilder

trait VectorInstances extends cats.kernel.instances.VectorInstances {
  implicit val catsStdInstancesForVector: TraverseFilter[Vector] with MonadCombine[Vector] with CoflatMap[Vector] with RecursiveTailRecM[Vector] =
    new TraverseFilter[Vector] with MonadCombine[Vector] with CoflatMap[Vector] with RecursiveTailRecM[Vector] {

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

      def traverseFilter[G[_], A, B](fa: Vector[A])(f: A => G[Option[B]])(implicit G: Applicative[G]): G[Vector[B]] =
        foldRight[A, G[Vector[B]]](fa, Always(G.pure(Vector.empty))){ (a, lgvb) =>
          G.map2Eval(f(a), lgvb)((ob, v) => ob.fold(v)(_ +: v))
        }.value

      def tailRecM[A, B](a: A)(fn: A => Vector[Either[A, B]]): Vector[B] = {
        val buf = Vector.newBuilder[B]
        var state = List(fn(a).iterator)
        @tailrec
        def loop(): Unit = state match {
          case Nil => ()
          case h :: tail if h.isEmpty =>
            state = tail
            loop()
          case h :: tail =>
            h.next match {
              case Right(b) =>
                buf += b
                loop()
              case Left(a) =>
                state = (fn(a).iterator) :: h :: tail
                loop()
            }
        }
        loop()
        buf.result
      }

      override def size[A](fa: Vector[A]): Long = fa.size.toLong

      override def traverse[G[_], A, B](fa: Vector[A])(f: A => G[B])(implicit G: Applicative[G]): G[Vector[B]] =
      foldRight[A, G[Vector[B]]](fa, Always(G.pure(Vector.empty))){ (a, lgvb) =>
        G.map2Eval(f(a), lgvb)(_ +: _)
      }.value

      override def exists[A](fa: Vector[A])(p: A => Boolean): Boolean =
        fa.exists(p)

      override def isEmpty[A](fa: Vector[A]): Boolean = fa.isEmpty

      override def filter[A](fa: Vector[A])(f: A => Boolean): Vector[A] = fa.filter(f)

      override def collect[A, B](fa: Vector[A])(f: PartialFunction[A, B]): Vector[B] = fa.collect(f)
    }

  implicit def catsStdShowForVector[A:Show]: Show[Vector[A]] =
    new Show[Vector[A]] {
      def show(fa: Vector[A]): String = fa.map(_.show).mkString("Vector(", ", ", ")")
    }
}
