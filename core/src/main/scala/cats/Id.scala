package cats

object Id {
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
      override def imap[A, B](fa: A)(f: A => B, fi: B => A): B = f(fa)
    }
}
