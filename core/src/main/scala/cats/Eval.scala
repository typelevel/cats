package cats

import scala.annotation.tailrec
import cats.syntax.all._

/**
 * Eval is a monad which controls evaluation.
 *
 * This type wraps a value (or a computation that produces a value)
 * and can produce it on command via the `.value` method.
 *
 * There are three basic evaluation strategies:
 *
 *  - Now:    evaluated immediately
 *  - Later:  evaluated once when value is needed
 *  - Always: evaluated every time value is needed
 *
 * The Later and Always are both lazy strategies while Now is eager.
 * Later and Always are distinguished from each other only by
 * memoization: once evaluated Later will save the value to be returned
 * immediately if it is needed again. Always will run its computation
 * every time.
 *
 * Eval supports stack-safe lazy computation via the .map and .flatMap
 * methods, which use an internal trampoline to avoid stack overflows.
 * Computation done within .map and .flatMap is always done lazily,
 * even when applied to a Now instance.
 *
 * It is not generally good style to pattern-match on Eval instances.
 * Rather, use .map and .flatMap to chain computation, and use .value
 * to get the result when needed. It is also not good style to create
 * Eval instances whose computation involves calling .value on another
 * Eval instance -- this can defeat the trampolining and lead to stack
 * overflows.
 */
sealed abstract class Eval[+A] extends Serializable { self =>

  /**
   * Evaluate the computation and return an A value.
   *
   * For lazy instances (Later, Always), any necessary computation
   * will be performed at this point. For eager instances (Now), a
   * value will be immediately returned.
   */
  def value: A

  /**
   * Transform an Eval[A] into an Eval[B] given the transformation
   * function `f`.
   *
   * This call is stack-safe -- many .map calls may be chained without
   * consumed additional stack during evaluation.
   *
   * Computation performed in f is always lazy, even when called on an
   * eager (Now) instance.
   */
  def map[B](f: A => B): Eval[B] =
    flatMap(a => Now(f(a)))

  /**
   * Lazily perform a computation based on an Eval[A], using the
   * function `f` to produce an Eval[B] given an A.
   *
   * This call is stack-safe -- many .flatMap calls may be chained
   * without consumed additional stack during evaluation. It is also
   * written to avoid left-association problems, so that repeated
   * calls to .flatMap will be efficiently applied.
   *
   * Computation performed in f is always lazy, even when called on an
   * eager (Now) instance.
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
      case c: Eval.Call[A] =>
        new Eval.Compute[B] {
          type Start = A
          val start = c.thunk
          val run = f
        }
      case _ =>
        new Eval.Compute[B] {
          type Start = A
          val start = () => self
          val run = f
        }
    }

  /**
   * Ensure that the result of the computation (if any) will be
   * memoized.
   *
   * Practically, this means that when called on an Always[A] a
   * Later[A] with an equivalent computation will be returned.
   */
  def memoize: Eval[A]
}


/**
 * Construct an eager Eval[A] instance.
 *
 * In some sense it is equivalent to using a val.
 *
 * This type should be used when an A value is already in hand, or
 * when the computation to produce an A value is pure and very fast.
 */
final case class Now[A](value: A) extends Eval[A] {
  def memoize: Eval[A] = this
}


/**
 * Construct a lazy Eval[A] instance.
 *
 * This type should be used for most "lazy" values. In some sense it
 * is equivalent to using a lazy val.
 *
 * When caching is not required or desired (e.g. if the value produced
 * may be large) prefer Always. When there is no computation
 * necessary, prefer Now.
 *
 * Once Later has been evaluated, the closure (and any values captured
 * by the closure) will not be retained, and will be available for
 * garbage collection.
 */
final class Later[A](f: () => A) extends Eval[A] {
  private[this] var thunk: () => A = f

  // The idea here is that `f` may have captured very large
  // structures, but produce a very small result. In this case, once
  // we've calculated a value, we would prefer to be able to free
  // everything else.
  //
  // (For situations where `f` is small, but the output will be very
  // expensive to store, consider using `Always`.)
  lazy val value: A = {
    val result = thunk()
    thunk = null // scalastyle:off
    result
  }

  def memoize: Eval[A] = this
}

object Later {
  def apply[A](a: => A): Later[A] = new Later(a _)
}

/**
 * Construct a lazy Eval[A] instance.
 *
 * This type can be used for "lazy" values. In some sense it is
 * equivalent to using a Function0 value.
 *
 * This type will evaluate the computation every time the value is
 * required. It should be avoided except when laziness is required and
 * caching must be avoided. Generally, prefer Later.
 */
final class Always[A](f: () => A) extends Eval[A] {
  def value: A = f()
  def memoize: Eval[A] = new Later(f)
}

object Always {
  def apply[A](a: => A): Always[A] = new Always(a _)
}

object Eval extends EvalInstances {

  /**
   * Construct an eager Eval[A] value (i.e. Now[A]).
   */
  def now[A](a: A): Eval[A] = Now(a)

  /**
   * Construct a lazy Eval[A] value with caching (i.e. Later[A]).
   */
  def later[A](a: => A): Eval[A] = new Later(a _)

  /**
   * Construct a lazy Eval[A] value without caching (i.e. Always[A]).
   */
  def always[A](a: => A): Eval[A] = new Always(a _)

  /**
   * Defer a computation which produces an Eval[A] value.
   *
   * This is useful when you want to delay execution of an expression
   * which produces an Eval[A] value. Like .flatMap, it is stack-safe.
   */
  def defer[A](a: => Eval[A]): Eval[A] =
    new Eval.Call[A](a _) {}

  /**
   * Static Eval instances for some common values.
   *
   * These can be useful in cases where the same values may be needed
   * many times.
   */
  val Unit: Eval[Unit] = Now(())
  val True: Eval[Boolean] = Now(true)
  val False: Eval[Boolean] = Now(false)
  val Zero: Eval[Int] = Now(0)
  val One: Eval[Int] = Now(1)

  /**
   * Call is a type of Eval[A] that is used to defer computations
   * which produce Eval[A].
   *
   * Users should not instantiate Call instances themselves. Instead,
   * they will be automatically created when needed.
   */
  sealed abstract class Call[A](val thunk: () => Eval[A]) extends Eval[A] {
    def memoize: Eval[A] = new Later(() => value)
    def value: A = Call.loop(this).value
  }

  object Call {
    /** Collapse the call stack for eager evaluations */
    @tailrec private def loop[A](fa: Eval[A]): Eval[A] = fa match {
      case call: Eval.Call[A] =>
        loop(call.thunk())
      case compute: Eval.Compute[A] =>
        new Eval.Compute[A] {
          type Start = compute.Start
          val start: () => Eval[Start] = () => compute.start()
          val run: Start => Eval[A] = s => loop1(compute.run(s))
        }
      case other => other
    }

    /**
     * Alias for loop that can be called in a non-tail position
     * from an otherwise tailrec-optimized loop.
     */
    private def loop1[A](fa: Eval[A]): Eval[A] = loop(fa)
  }

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
   * implementation of the .value method.
   */
  sealed abstract class Compute[A] extends Eval[A] {
    type Start
    val start: () => Eval[Start]
    val run: Start => Eval[A]

    def memoize: Eval[A] = Later(value)

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
                loop(c.run(xx.value), fs)
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

private[cats] trait EvalInstances extends EvalInstances0 {

  implicit val catsBimonadForEval: Bimonad[Eval] with Monad[Eval] with RecursiveTailRecM[Eval] =
    new Bimonad[Eval] with Monad[Eval] with RecursiveTailRecM[Eval] {
      override def map[A, B](fa: Eval[A])(f: A => B): Eval[B] = fa.map(f)
      def pure[A](a: A): Eval[A] = Now(a)
      def flatMap[A, B](fa: Eval[A])(f: A => Eval[B]): Eval[B] = fa.flatMap(f)
      def extract[A](la: Eval[A]): A = la.value
      def coflatMap[A, B](fa: Eval[A])(f: Eval[A] => B): Eval[B] = Later(f(fa))
      def tailRecM[A, B](a: A)(f: A => Eval[Either[A, B]]): Eval[B] = defaultTailRecM(a)(f)
    }

  implicit def catsOrderForEval[A: Order]: Order[Eval[A]] =
    new Order[Eval[A]] {
      def compare(lx: Eval[A], ly: Eval[A]): Int =
        lx.value compare ly.value
    }

  implicit def catsGroupForEval[A: Group]: Group[Eval[A]] =
    new EvalGroup[A] { val algebra: Group[A] = Group[A] }
}

private[cats] trait EvalInstances0 extends EvalInstances1 {
  implicit def catsPartialOrderForEval[A: PartialOrder]: PartialOrder[Eval[A]] =
    new PartialOrder[Eval[A]] {
      def partialCompare(lx: Eval[A], ly: Eval[A]): Double =
        lx.value partialCompare ly.value
    }

  implicit def catsMonoidForEval[A: Monoid]: Monoid[Eval[A]] =
    new EvalMonoid[A] { val algebra = Monoid[A] }
}

private[cats] trait EvalInstances1 {
  implicit def catsEqForEval[A: Eq]: Eq[Eval[A]] =
    new Eq[Eval[A]] {
      def eqv(lx: Eval[A], ly: Eval[A]): Boolean =
        lx.value === ly.value
    }

  implicit def catsSemigroupForEval[A: Semigroup]: Semigroup[Eval[A]] =
    new EvalSemigroup[A] { val algebra = Semigroup[A] }
}

trait EvalSemigroup[A] extends Semigroup[Eval[A]] {
  implicit def algebra: Semigroup[A]
  def combine(lx: Eval[A], ly: Eval[A]): Eval[A] =
    for { x <- lx; y <- ly } yield x |+| y
}

trait EvalMonoid[A] extends Monoid[Eval[A]] with EvalSemigroup[A] {
  implicit def algebra: Monoid[A]
  lazy val empty: Eval[A] = Eval.later(algebra.empty)
}

trait EvalGroup[A] extends Group[Eval[A]] with EvalMonoid[A] {
  implicit def algebra: Group[A]
  def inverse(lx: Eval[A]): Eval[A] =
    lx.map(_.inverse())
  override def remove(lx: Eval[A], ly: Eval[A]): Eval[A] =
    for { x <- lx; y <- ly } yield x |-| y
}
