package cats
package instances

import scala.annotation.tailrec
import cats.data.Xor

trait OptionInstances extends cats.kernel.instances.OptionInstances {

  implicit val catsStdInstancesForOption: Traverse[Option] with MonadError[Option, Unit] with MonadCombine[Option] with MonadRec[Option] with CoflatMap[Option] with Alternative[Option] =
    new Traverse[Option] with MonadError[Option, Unit]  with MonadCombine[Option] with MonadRec[Option] with CoflatMap[Option] with Alternative[Option] {

      def empty[A]: Option[A] = None

      def combineK[A](x: Option[A], y: Option[A]): Option[A] = x orElse y

      def pure[A](x: A): Option[A] = Some(x)

      override def map[A, B](fa: Option[A])(f: A => B): Option[B] =
        fa.map(f)

      def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
        fa.flatMap(f)

      @tailrec
      def tailRecM[A, B](a: A)(f: A => Option[A Xor B]): Option[B] =
        f(a) match {
          case None => None
          case Some(Xor.Left(a1)) => tailRecM(a1)(f)
          case Some(Xor.Right(b)) => Some(b)
        }

      override def map2[A, B, Z](fa: Option[A], fb: Option[B])(f: (A, B) => Z): Option[Z] =
        fa.flatMap(a => fb.map(b => f(a, b)))

      override def map2Eval[A, B, Z](fa: Option[A], fb: Eval[Option[B]])(f: (A, B) => Z): Eval[Option[Z]] =
        fa match {
          case None => Now(None)
          case Some(a) => fb.map(_.map(f(a, _)))
        }

      def coflatMap[A, B](fa: Option[A])(f: Option[A] => B): Option[B] =
        if (fa.isDefined) Some(f(fa)) else None

      def foldLeft[A, B](fa: Option[A], b: B)(f: (B, A) => B): B =
        fa match {
          case None => b
          case Some(a) => f(b, a)
        }

      def foldRight[A, B](fa: Option[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        fa match {
          case None => lb
          case Some(a) => f(a, lb)
        }

      def traverse[G[_]: Applicative, A, B](fa: Option[A])(f: A => G[B]): G[Option[B]] =
        fa match {
          case None => Applicative[G].pure(None)
          case Some(a) => Applicative[G].map(f(a))(Some(_))
        }

      def raiseError[A](e: Unit): Option[A] = None

      def handleErrorWith[A](fa: Option[A])(f: (Unit) => Option[A]): Option[A] = fa orElse f(())

      override def exists[A](fa: Option[A])(p: A => Boolean): Boolean =
        fa.exists(p)

      override def forall[A](fa: Option[A])(p: A => Boolean): Boolean =
        fa.forall(p)

      override def isEmpty[A](fa: Option[A]): Boolean =
        fa.isEmpty
    }

  implicit def catsStdShowForOption[A](implicit A: Show[A]): Show[Option[A]] =
    new Show[Option[A]] {
      def show(fa: Option[A]): String = fa match {
        case Some(a) => s"Some(${A.show(a)})"
        case None => "None"
      }
    }
}
