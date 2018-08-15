package cats
package instances

import cats.syntax.show._

import scala.annotation.tailrec
import scala.collection.+:
import scala.collection.immutable.VectorBuilder
import list._

trait VectorInstances extends cats.kernel.instances.VectorInstances {
  implicit val catsStdInstancesForVector: Traverse[Vector] with Monad[Vector] with Alternative[Vector] with CoflatMap[Vector] =
    new Traverse[Vector] with Monad[Vector] with Alternative[Vector] with CoflatMap[Vector] {

      def empty[A]: Vector[A] = Vector.empty[A]

      def combineK[A](x: Vector[A], y: Vector[A]): Vector[A] = x ++ y

      def pure[A](x: A): Vector[A] = Vector(x)

      override def map[A, B](fa: Vector[A])(f: A => B): Vector[B] =
        fa.map(f)

      def flatMap[A, B](fa: Vector[A])(f: A => Vector[B]): Vector[B] =
        fa.flatMap(f)

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

      override def foldMap[A, B](fa: Vector[A])(f: A => B)(implicit B: Monoid[B]): B =
        B.combineAll(fa.iterator.map(f))

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

      override def get[A](fa: Vector[A])(idx: Long): Option[A] =
        if (idx < Int.MaxValue && fa.size > idx && idx >= 0) Some(fa(idx.toInt)) else None

      override def traverse[G[_], A, B](fa: Vector[A])(f: A => G[B])(implicit G: Applicative[G]): G[Vector[B]] =
        foldRight[A, G[Vector[B]]](fa, Always(G.pure(Vector.empty))){ (a, lgvb) =>
          G.map2Eval(f(a), lgvb)(_ +: _)
        }.value

      override def mapWithIndex[A, B](fa: Vector[A])(f: (A, Int) => B): Vector[B] =
        fa.iterator.zipWithIndex.map(ai => f(ai._1, ai._2)).toVector

      override def zipWithIndex[A](fa: Vector[A]): Vector[(A, Int)] =
        fa.zipWithIndex

      override def exists[A](fa: Vector[A])(p: A => Boolean): Boolean =
        fa.exists(p)

      override def isEmpty[A](fa: Vector[A]): Boolean = fa.isEmpty

      override def foldM[G[_], A, B](fa: Vector[A], z: B)(f: (B, A) => G[B])(implicit G: Monad[G]): G[B] =
        Foldable[List].foldM(fa.toList, z)(f)

      override def fold[A](fa: Vector[A])(implicit A: Monoid[A]): A = A.combineAll(fa)

      override def toList[A](fa: Vector[A]): List[A] = fa.toList

      override def reduceLeftOption[A](fa: Vector[A])(f: (A, A) => A): Option[A] =
        fa.reduceLeftOption(f)

      override def find[A](fa: Vector[A])(f: A => Boolean): Option[A] = fa.find(f)

      override def algebra[A]: Monoid[Vector[A]] = new kernel.instances.VectorMonoid[A]

      override def collectFirst[A, B](fa: Vector[A])(pf: PartialFunction[A, B]): Option[B] = fa.collectFirst(pf)

      override def collectFirstSome[A, B](fa: Vector[A])(f: A => Option[B]): Option[B] = fa.collectFirst(Function.unlift(f))
    }

  implicit def catsStdShowForVector[A:Show]: Show[Vector[A]] =
    new Show[Vector[A]] {
      def show(fa: Vector[A]): String =
        fa.iterator.map(_.show).mkString("Vector(", ", ", ")")
    }
}

trait VectorInstancesBinCompat0 {
  implicit val catsStdTraverseEmptyForVector: TraverseEmpty[Vector] = new TraverseEmpty[Vector] {
    val traverse: Traverse[Vector] = cats.instances.vector.catsStdInstancesForVector

    override def mapFilter[A, B](fa: Vector[A])(f: (A) => Option[B]): Vector[B] =
      fa.collect(Function.unlift(f))

    override def filter[A](fa: Vector[A])(f: (A) => Boolean): Vector[A] = fa.filter(f)

    override def collect[A, B](fa: Vector[A])(f: PartialFunction[A, B]): Vector[B] = fa.collect(f)

    override def flattenOption[A](fa: Vector[Option[A]]): Vector[A] = fa.flatten

    def traverseFilter[G[_], A, B](fa: Vector[A])(f: (A) => G[Option[B]])(implicit G: Applicative[G]): G[Vector[B]] =
      fa.foldRight(Eval.now(G.pure(Vector.empty[B])))(
        (x, xse) => G.map2Eval(f(x), xse)((i, o) => i.fold(o)(_ +: o))
      ).value

    override def filterA[G[_], A](fa: Vector[A])(f: (A) => G[Boolean])(implicit G: Applicative[G]): G[Vector[A]] =
      fa.foldRight(Eval.now(G.pure(Vector.empty[A])))(
        (x, xse) =>
          G.map2Eval(f(x), xse)((b, vec) => if (b) x +: vec else vec)
      ).value
  }
}
