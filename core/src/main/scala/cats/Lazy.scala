package cats

sealed abstract class Lazy[A] { self =>
  def force: A

  def map[B](f: A => B): Lazy[B] =
    new Lazy[B] {
      def force: B = f(self.force)
    }

  def flatMap[B](f: A => Lazy[B]): Lazy[B] =
    new Lazy[B] {
      def force: B = f(self.force).force
    }
}

object Lazy {
  def apply[A](a: => A): Lazy[A] =
    new Lazy[A] {
      lazy val memo = a
      def force: A = memo
    }

  def memo[A](a: => A): Lazy[A] =
    apply(a)

  def eager[A](a: A): Lazy[A] =
    new Lazy[A] {
      def force: A = a
    }

  def byName[A](a: => A): Lazy[A] =
    new Lazy[A] {
      def force: A = a
    }

  implicit val lazyInstance: Bimonad[Lazy] =
    new Bimonad[Lazy] {

      def pure[A](a: A): Lazy[A] = Lazy.eager(a)

      def extract[A](fa: Lazy[A]): A =
        fa.force

      def flatMap[A, B](fa: Lazy[A])(f: A => Lazy[B]): Lazy[B] =
        fa.flatMap(f)

      def coflatMap[A, B](fa: Lazy[A])(f: Lazy[A] => B): Lazy[B] =
        Lazy(f(fa))

      override def map[A, B](fa: Lazy[A])(f: A => B): Lazy[B] =
        fa.map(f)

      override def apply[A, B](fa: Lazy[A])(ff: Lazy[A => B]): Lazy[B] =
        Lazy(ff.force(fa.force))

      override def flatten[A](ffa: Lazy[Lazy[A]]): Lazy[A] =
        Lazy.byName(ffa.force.force)

      override def map2[A, B, Z](fa: Lazy[A], fb: Lazy[B])(f: (A, B) => Z): Lazy[Z] =
        Lazy(f(fa.force, fb.force))

      override def fmap[A, B](f: A => B): Lazy[A] => Lazy[B] =
        la => la.map(f)

      override def imap[A, B](fa: Lazy[A])(f: A => B, fi: B => A): Lazy[B] =
        fa.map(f)
    }
}
