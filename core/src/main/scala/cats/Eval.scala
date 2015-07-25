package cats

import scala.annotation.tailrec
import cats.syntax.all._

/**
 * Eval is a monad which controls evaluation.
 *
 * This types wraps a value (or a computation that produces a value)
 * and can produce it on command via the `.value` method.
 *
 * There are three basic evaluation strategies:
 *
 *  - Eager:  evaluated immediately
 *  - Lazy:   evaluated once when value is needed
 *  - ByName: evaluated every time value is needed
 *
 * The Lazy and ByName are both lazy strategies while Eager is strict.
 * Lazy and ByName are distinguished from each other only by
 * memoization: once evaluated Lazy will save the value to be returned
 * immediately if it is needed again. ByName will run its computation
 * every time.
 *
 * Eval supports stack-safe lazy computation via the .map and .flatMap
 * methods, which use an internal trampoline to avoid stack overflows.
 * Computation done within .map and .flatMap is always done lazily,
 * even when applied to an Eager instance.
 *
 * It is not generally good style to pattern-match on Eval instances.
 * Rather, use .map and .flatMap to chain computation, and use .value
 * to get the result when needed. It is also not good style to create
 * Eval instances whose computation involves calling .value on another
 * Eval instance -- this can defeat the trampolining and lead to stack
 * overflows.
 */
sealed abstract class Eval[A] { self =>

  /**
   * Evaluate the computation and return an A value.
   *
   * For lazy instances, any necessary computation will be performed
   * at this point. For eager instances, a value will be immediately
   * returned.
   */
  def value: A

  /**
   * Transform an Eval[A] into an Eval[B] given the transformation
   * function `f`.
   *
   * This call is stack-safe -- many .map calls may be chained without
   * consumed additional stack during evaluation.
   */
  def map[B](f: A => B): Eval[B] =
    flatMap(a => Eager(f(a)))

  /**
   * Lazily perform a computation based on an Eval[A], using the
   * function `f` to produce an Eval[B] given an A.
   *
   * This call is stack-safe -- many .flatMap calls may be chained
   * without consumed additional stack during evaluation. It is also
   * written to avoid left-association problems, so that repeated
   * calls to .flatMap will be efficiently applied.
   */
  def flatMap[B](f: A => Eval[B]): Eval[B] =
    this match {
      case c: Eval.Compute[A] =>
        new Eval.Compute[B] {
          type Start = c.Start
          val start = c.start
          val run = (s: c.Start) =>
            new Eval.Compute[B] {
              type Start = A
              val start = () => c.run(s)
              val run = f
            }
        }
      case _ =>
        new Eval.Compute[B] {
          type Start = A
          val start = () => self
          val run = f
        }
    }
}

/**
 * Construct an eager Eval[A] instance.
 *
 * In some sense it is equivalent to using a val.
 *
 * This type should be used when an A value is already in hand, or
 * when the computation to produce an A value is pure and very fast.
 */
case class Eager[A](value: A) extends Eval[A]

/**
 * Construct a lazy Eval[A] instance.
 *
 * This type should be used for most "lazy" values. In some sense it
 * is equivalent to using a lazy val.
 *
 * When caching is not required or desired (e.g. if the value produced
 * may be large) prefer ByName. When there is no computation
 * necessary, prefer Eager.
 */
class Lazy[A](f: () => A) extends Eval[A] {
  lazy val value: A = f()
}

object Lazy {
  def apply[A](a: => A): Lazy[A] = new Lazy(a _)
}

/**
 * Construct a lazy Eval[A] instance.
 *
 * This type can be used for "lazy" values. In some sense it is
 * equivalent to using a Function0 value.
 *
 * This type will evaluate the computation every time the value is
 * required. It should be avoided except when laziness is required and
 * caching must be avoided. Generally, prefer Lazy.
 */
class ByName[A](f: () => A) extends Eval[A] {
  def value: A = f()
}

object ByName {
  def apply[A](a: => A): ByName[A] = new ByName(a _)
}

object Eval extends EvalInstances {

  /**
   * Construct an eager Eval[A] value (i.e. Eager[A]).
   */
  def eagerly[A](a: A): Eval[A] = Eager(a)

  /**
   * Construct a lazy Eval[A] value with caching (i.e. Lazy[A]).
   */
  def byNeed[A](a: => A): Eval[A] = new Lazy(a _)

  /**
   * Construct a lazy Eval[A] value without caching (i.e. ByName[A]).
   */
  def byName[A](a: => A): Eval[A] = new ByName(a _)

  /**
   * Defer a computation which produces an Eval[A] value.
   *
   * This is useful when you want to delay execution of an expression
   * which produces an Eval[A] value. Like .flatMap, it is stack-safe.
   */
  def defer[A](a: => Eval[A]): Eval[A] =
    Eval.Unit.flatMap(_ => a)

  /**
   * Static Eval instances for some common values.
   *
   * These can be useful in cases where the same values may be needed
   * many times.
   */
  val Unit: Eval[Unit] = Eager(())
  val True: Eval[Boolean] = Eager(true)
  val False: Eval[Boolean] = Eager(false)
  val Zero: Eval[Int] = Eager(0)
  val One: Eval[Int] = Eager(1)

  /**
   * Compute is a type of Eval[A] that is used to chain computations
   * involving .map and .flatMap. Along with Eval#flatMap it
   * implements the trampoline that guarantees stack-safety.
   *
   * Users should not instantiate Compute instances
   * themselves. Instead, they will be automatically created when
   * needed.
   *
   * Unlike a traditional trampoline, the internal workings of the
   * trampoline are not exposed. This allows a slightly more efficient
   * implementat of the .value method.
   */
  sealed abstract class Compute[A] extends Eval[A] {
    type Start
    val start: () => Eval[Start]
    val run: Start => Eval[A]

    def value: A = {
      type L = Eval[Any]
      type C = Any => Eval[Any]
      @tailrec def loop(curr: L, fs: List[C]): Any =
        curr match {
          case c: Compute[_] =>
            c.start() match {
              case cc: Compute[_] =>
                loop(
                  cc.start().asInstanceOf[L],
                  cc.run.asInstanceOf[C] :: c.run.asInstanceOf[C] :: fs)
              case xx =>
                loop(c.run(xx.value).asInstanceOf[L], fs)
            }
          case x =>
            fs match {
              case f :: fs => loop(f(x.value), fs)
              case Nil => x.value
            }
        }
      loop(this.asInstanceOf[L], Nil).asInstanceOf[A]
    }
  }
}

trait EvalInstances extends EvalInstances0 {

  implicit val evalBimonad: Bimonad[Eval] =
    new Bimonad[Eval] {
      override def map[A, B](fa: Eval[A])(f: A => B): Eval[B] = fa.map(f)
      def pure[A](a: A): Eval[A] = Eager(a)
      def flatMap[A, B](fa: Eval[A])(f: A => Eval[B]): Eval[B] = fa.flatMap(f)
      def extract[A](la: Eval[A]): A = la.value
      def coflatMap[A, B](fa: Eval[A])(f: Eval[A] => B): Eval[B] = Lazy(f(fa))
    }

  implicit def evalOrder[A: Order]: Eq[Eval[A]] =
    new Order[Eval[A]] {
      def compare(lx: Eval[A], ly: Eval[A]): Int =
        lx.value compare ly.value
    }

  implicit def evalGroup[A: Group]: Group[Eval[A]] =
    new EvalGroup[A] { val algebra = Group[A] }
}

trait EvalInstances0 extends EvalInstances1 {
  implicit def evalPartialOrder[A: PartialOrder]: Eq[Eval[A]] =
    new PartialOrder[Eval[A]] {
      def partialCompare(lx: Eval[A], ly: Eval[A]): Double =
        lx.value partialCompare ly.value
    }

  implicit def evalMonoid[A: Monoid]: Monoid[Eval[A]] =
    new EvalMonoid[A] { val algebra = Monoid[A] }
}

trait EvalInstances1 {
  implicit def evalEq[A: Eq]: Eq[Eval[A]] =
    new Eq[Eval[A]] {
      def eqv(lx: Eval[A], ly: Eval[A]): Boolean =
        lx.value === ly.value
    }

  implicit def evalSemigroup[A: Semigroup]: Semigroup[Eval[A]] =
    new EvalSemigroup[A] { val algebra = Semigroup[A] }
}

trait EvalSemigroup[A] extends Semigroup[Eval[A]] {
  implicit def algebra: Semigroup[A]
  def combine(lx: Eval[A], ly: Eval[A]): Eval[A] =
    for { x <- lx; y <- ly } yield x |+| y
}

trait EvalMonoid[A] extends Monoid[Eval[A]] with EvalSemigroup[A] {
  implicit def algebra: Monoid[A]
  lazy val empty: Eval[A] = Eval.byNeed(algebra.empty)
}

trait EvalGroup[A] extends Group[Eval[A]] with EvalMonoid[A] {
  implicit def algebra: Group[A]
  def inverse(lx: Eval[A]): Eval[A] =
    lx.map(_.inverse)
  override def remove(lx: Eval[A], ly: Eval[A]): Eval[A] =
    for { x <- lx; y <- ly } yield x |-| y
}
