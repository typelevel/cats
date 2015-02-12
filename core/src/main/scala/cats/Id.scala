package cats

/**
 * Identity, encoded as `type Id[A] = A`, a convenient alias to make
 * identity instances well-kinded.
 *
 * The identity monad can be seen as the ambient monad that encodes
 * the effect of having no effect. It is ambient in the sense that
 * plain pure values are values of `Id`.
 *
 * For instance, the [[cats.Functor]] instance for `[[cats.Id]]`
 * allows us to apply a function `A => B` to an `Id[A]` and get an
 * `Id[B]`. However, an `Id[A]` is the same as `A`, so all we're doing
 * is applying a pure function of type `A => B` to a pure value  of
 * type `A` to get a pure value of type `B`. That is, the instance
 * encodes pure unary function application.
 */
object Id {
  type Id[A] = A

  implicit val id: Bimonad[Id] =
    new Bimonad[Id] {
      def pure[A](a: A): A = a
      def extract[A](a: A): A = a
      def flatMap[A, B](a: A)(f: A => B): B = f(a)
      def coflatMap[A, B](a: A)(f: A => B): B = f(a)
      override def map[A, B](fa: A)(f: A => B): B = f(fa)
      override def apply[A, B](fa: A)(ff: A => B): B = ff(fa)
      override def flatten[A](ffa: A): A = ffa
      override def map2[A, B, Z](fa: A, fb: B)(f: (A, B) => Z): Z = f(fa, fb)
      override def fmap[A, B](f: A => B): A => B = f
      override def imap[A, B](fa: A)(f: A => B)(fi: B => A): B = f(fa)
    }
}
