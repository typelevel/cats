package cats
package std

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

      def foldRight[A, B](fa: Vector[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
        def loop(i: Int): Eval[B] =
          if (i < fa.length) f(fa(i), Eval.defer(loop(i + 1))) else lb
        Eval.defer(loop(0))
      }

      def traverse[G[_]: Applicative, A, B](fa: Vector[A])(f: A => G[B]): G[Vector[B]] = {
        val G = Applicative[G]
        val gba = G.pure(Vector.empty[B])
        fa.foldLeft(gba)((buf, a) => G.map2(buf, f(a))(_ :+ _))
      }

      override def exists[A](fa: Vector[A])(p: A => Boolean): Boolean =
        fa.exists(p)

      override def isEmpty[A](fa: Vector[A]): Boolean = fa.isEmpty
    }

  // TODO: eventually use algebra's instances (which will deal with
  // implicit priority between Eq/PartialOrder/Order).

  implicit def eqVector[A](implicit ev: Eq[A]): Eq[Vector[A]] =
    new Eq[Vector[A]] {
      def eqv(x: Vector[A], y: Vector[A]): Boolean = {
        def loop(xs: Vector[A], ys: Vector[A]): Boolean =
          xs match {
            case Seq() => ys.isEmpty
            case a +: xs =>
              ys match {
                case Seq() => false
                case b +: ys => if (ev.neqv(a, b)) false else loop(xs, ys)
              }
          }
        loop(x, y)
      }
    }
}
