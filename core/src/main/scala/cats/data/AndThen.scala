package cats.data

import java.io.Serializable

/**
 * A function type of a single input that can do function composition
 * (via `andThen` and `compose`) in constant stack space with amortized
 * linear time application (in the number of constituent functions).
 *
 * Example:
 *
 * {{{
 *   val seed = AndThen((x: Int) => x + 1))
 *   val f = (0 until 10000).foldLeft(seed)((acc, _) => acc.andThen(_ + 1))
 *
 *   // This should not trigger stack overflow ;-)
 *   f(0)
 * }}}
 */
private[cats] sealed abstract class AndThen[-T, +R]
  extends (T => R) with Product with Serializable {

  import AndThen._

  final def apply(a: T): R =
    runLoop(a)

  override def andThen[A](g: R => A): AndThen[T, A] = {
    // Fusing calls up to a certain threshold, using the fusion
    // technique implemented for `cats.effect.IO#map`
    this match {
      case Single(f, index) if index != fusionMaxStackDepth =>
        Single(f.andThen(g), index + 1)
      case _ =>
        andThenF(AndThen(g))
    }
  }

  override def compose[A](g: A => T): AndThen[A, R] = {
    // Fusing calls up to a certain threshold, using the fusion
    // technique implemented for `cats.effect.IO#map`
    this match {
      case Single(f, index) if index != fusionMaxStackDepth =>
        Single(f.compose(g), index + 1)
      case _ =>
        composeF(AndThen(g))
    }
  }

  private def runLoop(start: T): R = {
    var self: AndThen[Any, Any] = this.asInstanceOf[AndThen[Any, Any]]
    var current: Any = start.asInstanceOf[Any]
    var continue = true

    while (continue) {
      self match {
        case Single(f, _) =>
          current = f(current)
          continue = false

        case Concat(Single(f, _), right) =>
          current = f(current)
          self = right.asInstanceOf[AndThen[Any, Any]]

        case Concat(left @ Concat(_, _), right) =>
          self = left.rotateAccum(right)
      }
    }
    current.asInstanceOf[R]
  }

  private final def andThenF[X](right: AndThen[R, X]): AndThen[T, X] =
    Concat(this, right)
  private final def composeF[X](right: AndThen[X, T]): AndThen[X, R] =
    Concat(right, this)

  // converts left-leaning to right-leaning
  protected final def rotateAccum[E](_right: AndThen[R, E]): AndThen[T, E] = {
    var self: AndThen[Any, Any] = this.asInstanceOf[AndThen[Any, Any]]
    var right: AndThen[Any, Any] = _right.asInstanceOf[AndThen[Any, Any]]
    var continue = true
    while (continue) {
      self match {
        case Concat(left, inner) =>
          self = left.asInstanceOf[AndThen[Any, Any]]
          right = inner.andThenF(right)

        case _ => // Single
          self = self.andThenF(right)
          continue = false
      }
    }
    self.asInstanceOf[AndThen[T, E]]
  }

  override def toString: String =
    "AndThen$" + System.identityHashCode(this)
}

private[cats] object AndThen {
  /** Builds an [[AndThen]] reference by wrapping a plain function. */
  def apply[A, B](f: A => B): AndThen[A, B] =
    f match {
      case ref: AndThen[A, B] @unchecked => ref
      case _ => Single(f, 0)
    }

  private final case class Single[-A, +B](f: A => B, index: Int)
    extends AndThen[A, B]
  private final case class Concat[-A, E, +B](left: AndThen[A, E], right: AndThen[E, B])
    extends AndThen[A, B]

  /** 
   * Establishes the maximum stack depth when fusing `andThen` or 
   * `compose` calls.
   *
   * The default is `128`, from which we substract one as an optimization,
   * a "!=" comparisson being slightly more efficient than a "<".
   *
   * This value was reached by taking into account the default stack 
   * size as set on 32 bits or 64 bits, Linux or Windows systems,
   * being enough to notice performance gains, but not big enough
   * to be in danger of triggering a stack-overflow error.
   */
  private final val fusionMaxStackDepth = 127
}
