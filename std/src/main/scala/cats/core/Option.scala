package cats
package std

trait option {
  implicit val optionInstances: MonadCombine[Option] with MonadFilter[Option] with MonoidK[Option] with CoFlatMap[Option] with Traverse[Option] =
    new MonadCombine[Option] with MonadFilter[Option] with MonoidK[Option] with CoFlatMap[Option] with Traverse[Option] {

      // Members declared in cats.Applicative
      def pure[A](x: A): Option[A] = Some(x)

      // Members declared in cats.CoFlatMap
      def coflatMap[A, B](fa: Option[A])(f: Option[A] => B): Option[B] = Some(f(fa))

      // Members declared in cats.FlatMap
      def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa flatMap f

      // Members declared in cats.Foldable
      def foldLeft[A, B](fa: Option[A],b: B)(f: (B, A) => B): B = fa match {
        case Some(a) => f(b,a)
        case None => b
      }

      def foldRight[A, B](fa: Option[A],b: cats.Lazy[B])(f: (A, cats.Lazy[B]) => B): cats.Lazy[B] = fa match {
        case Some(a) => Lazy(f(a,b))
        case None => b
      }
      def foldRight[A, B](fa: Option[A],b: B)(f: (A, B) => B): B = fa match {
        case Some(a) => f(a,b)
        case None => b
      }

      // Members declared in cats.MonoidK
      def empty[A]: Option[A] = None

      // Members declared in cats.SemigroupK
      def combine[A](x: Option[A],y: Option[A]): Option[A] = x orElse y

      // Members declared in cats.Traverse
      def traverse[F[_], A, B](fa: Option[A])(f: A => F[B])(implicit F: Applicative[F]): F[Option[B]] =
        fa match {
          case Some(a) => F.map(f(a))(Some(_))
          case None => F.pure(None)
        }
    }
}


