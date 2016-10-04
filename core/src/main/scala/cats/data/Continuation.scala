package cats
package data

import java.io.Serializable
import scala.annotation.tailrec

sealed abstract class Continuation[O, +I] extends Serializable {
  def map[I2](fn: I => I2): Continuation[O, I2] =
    Continuation.Mapped(this, fn)

  def flatMap[I2](fn: I => Continuation[O, I2]): Continuation[O, I2] =
    Continuation.FlatMapped(this, fn)

  final def apply(fn: I => O): O = {
    import Continuation._
    val re = new RunEnv[O]
    re.eval(this, re.ScalaFn(fn)).get
  }
}

object Continuation {
  def pure[O] = new PureBuilder[O]
  class PureBuilder[O] {
    def apply[I](i: I): Continuation[O, I] = Const(i)
  }
  def from[I, O](fn: (I => O) => O): Continuation[O, I] = Cont(fn)

  implicit def catsDataContinuationMonad[O]: Monad[Continuation[O, ?]] = new Monad[Continuation[O, ?]] {
    def pure[I](i: I): Continuation[O, I] = Const(i)
    override def map[I1, I2](f: Continuation[O, I1])(fn: I1 => I2): Continuation[O, I2] = f.map(fn)
    def flatMap[I1, I2](f: Continuation[O, I1])(fn1: I1 => Continuation[O, I2]): Continuation[O, I2] = f.flatMap(fn1)
    /**
     * flatMap is trampolined, BUT the stack depth grows proportional to the number of `from` calls
     */
    def tailRecM[A, B](a: A)(fn: A => Continuation[O, Either[A, B]]): Continuation[O, B] =
      fn(a).flatMap {
        case Right(b) => Const(b)
        case Left(a) => tailRecM(a)(fn)
      }
  }

  private case class Const[O, I](i: I) extends Continuation[O, I]

  private case class Mapped[O, I1, I2](
    c: Continuation[O, I1],
    fn: I1 => I2) extends Continuation[O, I2]

  private case class FlatMapped[O, I1, I2](
    c: Continuation[O, I1],
    fn: I1 => Continuation[O, I2]) extends Continuation[O, I2]

  private case class Cont[O, I](runfn: (I => O) => O) extends Continuation[O, I]

  /**
   * We hide all our implementation in this class. Note there is only
   * ever one output, so we put the type here.
   */
  private class RunEnv[O] {
    sealed abstract class Result {
      def get: O
    }
    case class Complete(get: O) extends Result
    case class ContResult[I](runfn: (I => O) => O, fn: Fn[I]) extends Result {
      def get: O = fn match {
        case ScalaFn(sfn) => runfn(sfn)
        case _ =>
          // This is not stack safe, we could keep finding continuations
          // the stack depth will scale as the number of inner continuations,
          // not flatMaps (which you can see
          runfn { i: I => eval(Const(i), fn).get }
      }
    }

    /**
     * This evaluates a continuation given an Fn
     */
    final def eval[I](c: Continuation[O, I], fn: Fn[I]): Result =
      loop(c.asInstanceOf[Continuation[O, Any]], fn.asInstanceOf[Fn[Any]])

    /**
     * This is the tail recursive loop that partially evaluates until
     * we reach the next continuation. Note, it compiles with the type below:
     *
     * final def loop[I](c: Continuation[O, I], fn: Fn[I]): Result = c match {
     *
     * but we erase I to Any so scala tailrec optimization can work
     */
    @tailrec
    private def loop(c: Continuation[O, Any], fn: Fn[Any]): Result = c match {
      case Cont(k) => ContResult(k, fn)
      case Mapped(c, mfn) => loop(c, AndThen(mfn, fn))
      case FlatMapped(c, next) => loop(c, RunCont(next, fn))
      case Const(i) => fn match {
        case ScalaFn(sfn) => Complete(sfn(i))
        case RunCont(mkC, next0) => loop(mkC(i), next0)
        case AndThen(sfn, next) => loop(Const(sfn(i)), next)
      }
    }

    sealed abstract class Fn[-A]
    case class ScalaFn[A](fn: A => O) extends Fn[A]
    case class RunCont[A, B](fn: A => Continuation[O, B], next: Fn[B]) extends Fn[A]
    case class AndThen[A, B](fn: A => B, next: Fn[B]) extends Fn[A]
  }
}
