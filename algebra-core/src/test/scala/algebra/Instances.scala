package algebra

object Instances {

  def t2HasSemigroup[A, B](implicit eva: Semigroup[A], evb: Semigroup[B]) =
    new Semigroup[(A, B)] {
      def combine(x: (A, B), y: (A, B)): (A, B) =
        (eva.combine(x._1, y._1), evb.combine(x._2, y._2))
    }

  val stringHasMonoid =
    new Monoid[String] {
      def empty: String = ""
      def combine(x: String, y: String): String = x + y
    }

  def f1ComposeMonoid[A] =
    new Monoid[A => A] {
      def empty: A => A =
        a => a
      def combine(x: A => A, y: A => A): A => A =
        a => y(x(a))
    }

  def f1HomomorphismMonoid[A, B](implicit ev: Monoid[B]) =
    new Monoid[A => B] {
      def empty: A => B =
        _ => ev.empty
      def combine(x: A => B, y: A => B): A => B =
        a => ev.combine(x(a), y(a))
    }
}
