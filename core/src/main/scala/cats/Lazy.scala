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
sealed abstract class Lazy[A] extends Product with Serializable { self =>

  import Lazy.{byNeed, ByNeed, ByName}

  /**
   * Obtain the underlying value from this lazy instance. If the value
   * has already been calculated, it will be returned. Otherwise, it
   * will be calculated and returned (and optionally memoized).
   */
  def value: A

  /**
   * Given a lazy value, create a new one which will cache
   * (i.e. memoize) its value.
   *
   * The practical effect of this method is to convert by-name
   * instances to by-need (since eager instances already have a
   * memoized value).
   */
  def cached: Lazy[A] =
    this match {
      case ByName(f) => byNeed(f())
      case _ => this
    }

  /**
   * Given a lazy value, create a new one which will not cache its
   * value (forgetting a cached value if any).
   *
   * The practical effect of this method is to convert by-need
   * instances to by-name (eager instances have no way to recalculate
   * their value so they are unaffected).
   */
  def uncached: Lazy[A] =
    this match {
      case need @ ByNeed() => ByName(() => need.value)
      case _ => this
    }
}

object Lazy {

  case class Eager[A](value: A) extends Lazy[A]

  case class ByName[A](f: () => A) extends Lazy[A] {
    def value: A = f()
  }

  private abstract case class ByNeed[A]() extends Lazy[A]

  /**
   * Construct a lazy value.
   *
   * This instance will be call-by-need (`body` will not be evaluated
   * until needed).
   */
  def apply[A](body: => A): Lazy[A] =
    byNeed(body)

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
    new ByNeed[A] {
      override lazy val value = body
    }

  implicit def lazyEq[A: Eq]: Eq[Lazy[A]] =
    Eq.by(_.value)
}
