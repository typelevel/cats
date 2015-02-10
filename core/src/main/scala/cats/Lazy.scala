package cats

/**
 * Represents a value which may not yet be evaluated.
 *
 * Lazy provides a method to abstract across the evaluation strategy
 * in Scala. There are three supported strategies:
 *
 *  - `Lazy(...)`: call-by-need semantics; the value of `...` will not
 *     be calculated until needed, but will be calculated at most once
 *     (and saved via memoization). Corresponds to Scala's `lazy val`.
 *
 *  - `Lazy.eager(...)`: call-by-value semantics; the value of `...`
 *    will be immediately calculated and saved. This is the default
 *    strategy used by Scala. Corresponds to Scala's `val`.
 *
 *  - `Lazy.byName(...)`: call-by-name semantics; the value of `...`
 *    will not be calculated until needed, and will be calculated
 *    every time it is needed. Corresponds to Scala's `def`.
 *
 * Every Lazy[A] value has (or can calculate) a corresponding A
 * value. You can obtain this value by calling the `.value` method.
 */
sealed abstract class Lazy[A] { self =>

  import Lazy.{ByNeed, ByName, Eager}

  /**
   * Obtain the underlying value from this lazy instance. If the value
   * has already been calculated, it will be returned. Otherwise, it
   * will be calculated and returned (and optionally memoized).
   */
  def value: A

  /**
   * Lazily-apply the given function to this lazy value.
   *
   * The resulting `Lazy[B]` value is call-by-need.
   */
  def map[B](f: A => B): Lazy[B] =
    ByNeed(() => f(self.value))

  /**
   * Lazily-apply the given function to this lazy value.
   *
   * The resulting `Lazy[B]` value is call-by-need.
   */
  def flatMap[B](f: A => Lazy[B]): Lazy[B] =
    ByNeed(() => f(self.value).value)

  /**
   * Given a lazy value, create a new one which will memoize its
   * value.
   *
   * The practical effect of this method is to convert by-name
   * instances to by-need (since eager instances already have a
   * memoized value).
   */
  def memoize: Lazy[A] =
    this match {
      case ByName(f) => ByNeed(f)
      case _ => this
    }
}

object Lazy extends LazyInstances {

  case class Eager[A](value: A) extends Lazy[A]

  case class ByName[A](f: () => A) extends Lazy[A] {
    def value: A = f()
  }

  case class ByNeed[A](f: () => A) extends Lazy[A] {
    lazy val memo = f()
    def value: A = memo
  }

  /**
   * Construct a lazy value.
   *
   * This instance will be call-by-need (`body` will not be evaluated
   * until needed).
   */
  def apply[A](body: => A): Lazy[A] =
    ByNeed(body _)

  /**
   * Construct a lazy value.
   *
   * This instance will be call-by-value (`a` will have already been
   * evaluated).
   */
  def eager[A](a: A): Lazy[A] =
    Eager(a)

  /**
   * Construct a lazy value.
   *
   * This instance will be call-by-name (`body` will not be evaluated
   * until needed).
   */
  def byName[A](body: => A): Lazy[A] =
    ByName(body _)

  /**
   * Alias for `apply`, to mirror the `byName` method.
   */
  def byNeed[A](body: => A): Lazy[A] =
    ByNeed(body _)
}

trait LazyInstances extends LazyInstances1 {
  implicit val lazyInstance: Bimonad[Lazy] =
    new Bimonad[Lazy] {

      def pure[A](a: A): Lazy[A] = Lazy.eager(a)

      def extract[A](fa: Lazy[A]): A =
        fa.value

      def flatMap[A, B](fa: Lazy[A])(f: A => Lazy[B]): Lazy[B] =
        fa.flatMap(f)

      def coflatMap[A, B](fa: Lazy[A])(f: Lazy[A] => B): Lazy[B] =
        Lazy(f(fa))

      override def map[A, B](fa: Lazy[A])(f: A => B): Lazy[B] =
        fa.map(f)

      override def apply[A, B](fa: Lazy[A])(ff: Lazy[A => B]): Lazy[B] =
        Lazy(ff.value(fa.value))

      override def flatten[A](ffa: Lazy[Lazy[A]]): Lazy[A] =
        Lazy.byName(ffa.value.value)

      override def map2[A, B, Z](fa: Lazy[A], fb: Lazy[B])(f: (A, B) => Z): Lazy[Z] =
        Lazy(f(fa.value, fb.value))

      override def fmap[A, B](f: A => B): Lazy[A] => Lazy[B] =
        la => la.map(f)

      override def imap[A, B](fa: Lazy[A])(f: A => B)(fi: B => A): Lazy[B] =
        fa.map(f)
    }

  implicit def lazyOrder[A](implicit A: Order[A]): Order[Lazy[A]] =
    new Order[Lazy[A]] {
      def compare(x: Lazy[A], y: Lazy[A]): Int = A.compare(x.value, y.value)
    }
}

trait LazyInstances1 extends LazyInstances0 {
  implicit def lazyPartialOrder[A](implicit A: PartialOrder[A]): PartialOrder[Lazy[A]] =
    new PartialOrder[Lazy[A]] {
      def partialCompare(x: Lazy[A], y: Lazy[A]): Double =
        A.partialCompare(x.value, y.value)
    }
}

trait LazyInstances0 {
  implicit def lazyEq[A](implicit A: Eq[A]): Eq[Lazy[A]] =
    new Eq[Lazy[A]] {
      def eqv(x: Lazy[A], y: Lazy[A]): Boolean = A.eqv(x.value, y.value)
    }
}
