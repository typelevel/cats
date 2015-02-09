package cats

/**
 * Fold is designed to allow laziness/short-circuiting in foldRight.
 *
 * It is a sum type that has three possible subtypes:
 *
 *   - `Return(a)`: stop the fold with a value of `a`.
 *   - `Continue(f)`: continue the fold, suspending the computation `f` for this step.
 *   - `Pass`: continue the fold, with no computation for this step.
 *
 * The meaning of these types can be made more clear with an example
 * of the foldRight method in action. Here's a method to count how many
 * elements appear in a list before the value 3:
 *
 * {{{
 *     def f(n: Int): Fold[Int] =
 *       if (n == 3) Fold.Return(0) else Fold.Continue(_ + 1)
 *
 *     val count: Lazy[Int] = List(1,2,3,4).foldRight(Lazy(0))(f)
 * }}}
 *
 * When we call `count.value`, the following occurs:
 *
 *  - `f(1)` produces `res0: Continue(_ + 1)`
 *  - `f(2)` produces `res1: Continue(_ + 1)`
 *  - `f(3)` produces `res2: Return(0)`
 *
 * Now we unwind back through the continue instances:
 *
 *  - `res2` returns `0`
 *  - `res1(0)` returns `1`
 *  - `res0(1)` returns `2`
 *
 * And so the result is 2.
 *
 * This code searches an infinite stream for 77:
 *
 * {{{
 *    val found: Lazy[Boolean] =
 *      Stream.from(0).foldRight(Lazy(false)) { n =>
 *        if (n == 77) Fold.Return(true) else Fold.Pass
 *      }
 * }}}
 *
 * Here's another example that sums the list until it reaches a
 * negative number:
 *
 * {{{
 *    val sum: Lazy[Double] =
 *      numbers.foldRight(Lazy(0.0)) { n =>
 *        if (n < 0) Fold.Return(0.0) else Fold.Continue(n + _)
 *      }
 * }}}
 *
 * This strange example counts an infinite stream. Since the result is
 * lazy, it will only hang the program once `count.value` is called:
 *
 * {{{
 *    val count: Lazy[Long] =
 *      Stream.from(0).foldRight(Lazy(0L)) { _ =>
 *        Fold.Continue(_ + 1L)
 *      }
 * }}}
 *
 * You can even implement foldLeft in terms of foldRight (!):
 *
 * {{{
 *    def foldl[A, B](as: List[A], b: B)(f: (B, A) => B): B =
 *      as.foldRight(Lazy((b: B) => b)) { a =>
 *        Fold.Continue(g => (b: B) => g(f(b, a)))
 *      }.value(b)
 * }}}
 *
 * (In practice you would not want to use the `foldl` because it is
 * not stack-safe.)
 */
sealed abstract class Fold[A] extends Product with Serializable {
  import Fold.{Return, Continue, Pass}

  def imap[B](f: A => B)(g: B => A): Fold[B] =
    this match {
      case Return(a) => Return(f(a))
      case Continue(h) => Continue(b => f(g(b)))
      case _ => Pass
    }

  def compose(f: A => A): Fold[A] =
    this match {
      case Return(a) => Return(f(a))
      case Continue(g) => Continue(f andThen g)
      case _ => Continue(f)
    }

  def complete(la: Lazy[A]): A =
    this match {
      case Return(a) => a
      case Continue(f) => f(la.value)
      case _ => la.value
    }
}

object Fold {

  /**
   * Return signals that the "rest" of a fold can be ignored.
   *
   * Crucially, the `a` value here is not necessarily the value that
   * will be returned from foldRight, but instead it is the value that
   * will be returned to the previous functions provided by any Continue
   * instances.
   */
  final case class Return[A](a: A) extends Fold[A]

  /**
   * Continue suspends a calculation, allowing the fold to continue.
   *
   * When the end of the fold is reached, the final A value will
   * propagate back through these functions, producing a final result.
   */
  final case class Continue[A](f: A => A) extends Fold[A]

  /**
   * Pass allows the fold to continue, without modifying the result.
   *
   * Pass' behavior is identical to `Continue(identity[A])`, but it may be
   * more efficient.
   */
  final def Pass[A]: Fold[A] = pass.asInstanceOf[Fold[A]]

  final case object pass extends Fold[Nothing]

  /**
   * partialIterate provides a partialFold for `Iterable[A]` values.
   */
  def partialIterate[A, B](as: Iterable[A])(f: A => Fold[B]): Fold[B] = {
    def unroll(b: B, fs: List[B => B]): B =
      fs.foldLeft(b)((b, f) => f(b))
    def loop(it: Iterator[A], fs: List[B => B]): Fold[B] =
      if (it.hasNext) {
        f(it.next) match {
          case Return(b) => Return(unroll(b, fs))
          case Continue(f) => loop(it, f :: fs)
          case _ => loop(it, fs)
        }
      } else Continue(b => unroll(b, fs))
    loop(as.iterator, Nil)
  }
}
