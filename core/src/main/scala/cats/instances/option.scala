package cats
package instances

import scala.annotation.tailrec

trait OptionInstances extends cats.kernel.instances.OptionInstances {

  implicit val catsStdInstancesForOption: Traverse[Option]
    with MonadError[Option, Unit]
    with Alternative[Option]
    with CommutativeMonad[Option]
    with CoflatMap[Option] =
    new Traverse[Option]
      with MonadError[Option, Unit]
      with Alternative[Option]
      with CommutativeMonad[Option]
      with CoflatMap[Option] {

      def empty[A]: Option[A] = None

      def combineK[A](x: Option[A], y: Option[A]): Option[A] = x.orElse(y)

      def pure[A](x: A): Option[A] = Some(x)

      override def map[A, B](fa: Option[A])(f: A => B): Option[B] =
        fa.map(f)

      def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
        fa.flatMap(f)

      @tailrec
      def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] =
        f(a) match {
          case None           => None
          case Some(Left(a1)) => tailRecM(a1)(f)
          case Some(Right(b)) => Some(b)
        }

      override def map2[A, B, Z](fa: Option[A], fb: Option[B])(f: (A, B) => Z): Option[Z] =
        fa.flatMap(a => fb.map(b => f(a, b)))

      override def map2Eval[A, B, Z](fa: Option[A], fb: Eval[Option[B]])(f: (A, B) => Z): Eval[Option[Z]] =
        fa match {
          case None    => Now(None)
          case Some(a) => fb.map(_.map(f(a, _)))
        }

      def coflatMap[A, B](fa: Option[A])(f: Option[A] => B): Option[B] =
        if (fa.isDefined) Some(f(fa)) else None

      def foldLeft[A, B](fa: Option[A], b: B)(f: (B, A) => B): B =
        fa match {
          case None    => b
          case Some(a) => f(b, a)
        }

      def foldRight[A, B](fa: Option[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        fa match {
          case None    => lb
          case Some(a) => f(a, lb)
        }

      def raiseError[A](e: Unit): Option[A] = None

      def handleErrorWith[A](fa: Option[A])(f: (Unit) => Option[A]): Option[A] = fa.orElse(f(()))

      def traverse[G[_]: Applicative, A, B](fa: Option[A])(f: A => G[B]): G[Option[B]] =
        fa match {
          case None    => Applicative[G].pure(None)
          case Some(a) => Applicative[G].map(f(a))(Some(_))
        }

      override def reduceLeftToOption[A, B](fa: Option[A])(f: A => B)(g: (B, A) => B): Option[B] =
        fa.map(f)

      override def reduceRightToOption[A, B](fa: Option[A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[Option[B]] =
        Now(fa.map(f))

      override def reduceLeftOption[A](fa: Option[A])(f: (A, A) => A): Option[A] = fa

      override def reduceRightOption[A](fa: Option[A])(f: (A, Eval[A]) => Eval[A]): Eval[Option[A]] =
        Now(fa)

      override def minimumOption[A](fa: Option[A])(implicit A: Order[A]): Option[A] = fa

      override def maximumOption[A](fa: Option[A])(implicit A: Order[A]): Option[A] = fa

      override def get[A](fa: Option[A])(idx: Long): Option[A] =
        if (idx == 0L) fa else None

      override def size[A](fa: Option[A]): Long = fa.fold(0L)(_ => 1L)

      override def foldMap[A, B](fa: Option[A])(f: A => B)(implicit B: Monoid[B]): B =
        fa.fold(B.empty)(f)

      override def find[A](fa: Option[A])(f: A => Boolean): Option[A] =
        fa.filter(f)

      override def exists[A](fa: Option[A])(p: A => Boolean): Boolean =
        fa.exists(p)

      override def forall[A](fa: Option[A])(p: A => Boolean): Boolean =
        fa.forall(p)

      override def toList[A](fa: Option[A]): List[A] = fa.toList

      override def filter_[A](fa: Option[A])(p: A => Boolean): List[A] =
        fa.filter(p).toList

      override def takeWhile_[A](fa: Option[A])(p: A => Boolean): List[A] =
        fa.filter(p).toList

      override def dropWhile_[A](fa: Option[A])(p: A => Boolean): List[A] =
        fa.filterNot(p).toList

      override def isEmpty[A](fa: Option[A]): Boolean =
        fa.isEmpty

      override def collectFirst[A, B](fa: Option[A])(pf: PartialFunction[A, B]): Option[B] = fa.collectFirst(pf)

      override def collectFirstSome[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
    }

  implicit def catsStdShowForOption[A](implicit A: Show[A]): Show[Option[A]] =
    new Show[Option[A]] {
      def show(fa: Option[A]): String = fa match {
        case Some(a) => s"Some(${A.show(a)})"
        case None    => "None"
      }
    }
}

private[instances] trait OptionInstancesBinCompat0 {
  implicit val catsStdTraverseFilterForOption: TraverseFilter[Option] = new TraverseFilter[Option] {
    val traverse: Traverse[Option] = cats.instances.option.catsStdInstancesForOption

    override def mapFilter[A, B](fa: Option[A])(f: (A) => Option[B]): Option[B] = fa.flatMap(f)

    override def filter[A](fa: Option[A])(f: (A) => Boolean): Option[A] = fa.filter(f)

    override def collect[A, B](fa: Option[A])(f: PartialFunction[A, B]): Option[B] = fa.collect(f)

    override def flattenOption[A](fa: Option[Option[A]]): Option[A] = fa.flatten

    def traverseFilter[G[_], A, B](fa: Option[A])(f: (A) => G[Option[B]])(implicit G: Applicative[G]): G[Option[B]] =
      fa match {
        case None    => G.pure(Option.empty[B])
        case Some(a) => f(a)
      }

    override def filterA[G[_], A](fa: Option[A])(f: (A) => G[Boolean])(implicit G: Applicative[G]): G[Option[A]] =
      fa match {
        case None    => G.pure(Option.empty[A])
        case Some(a) => G.map(f(a))(b => if (b) Some(a) else None)
      }

  }
}
