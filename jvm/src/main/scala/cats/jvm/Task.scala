package cats
package jvm

import cats.data.Xor
import scala.reflect.ClassTag
import java.util.concurrent.{Callable, CountDownLatch, ExecutorService, Executors}

/**
 * A Task is an abstraction of a computation which produces an A
 * possibly asynchronously
 */
final class Task[A](private val eval: Eval[A]) { self =>
  import Task._

  def map[B](f: A => B): Task[B] =
    new Task(eval map f)

  def flatMap[B](f: A => Task[B]): Task[B] =
    new Task(Eval.defer(eval.flatMap(f andThen (_.eval))))

  /**
   * Returns a new Task which will evaluate the computation in this
   * Task, but catch any non-fatal exception thrown by the running of
   * the computation.
   */
  def catchNonFatal: Task[Throwable Xor A] =
    new Task(Eval.always(Xor.catchNonFatal(eval.value)))

  /**
   * Returns a new Task which will evaluate the computation in this
   * Task, but catch some subset of the exceptions that might be
   * thrown by the running of the computation.
   */
  def catchOnly[T >: Null <: Throwable: ClassTag]: Task[T Xor A] =
    new Task(Eval.always(Xor.catchOnly[T](eval.value)))

  /**
   * Run the computation to produce an A
   */
  final def unsafePerformIO(): A = eval.value

  /**
   * Run this computation asynchronously, and call the callback with the result
   */
  final def unsafeAsyncIO(cb: A => Unit)(implicit pool: ExecutorService): Unit = {
    val _ = pool.submit(new Callable[Unit] {
      def call = cb(Task.this.unsafePerformIO)
    })
  }

  /**
   * Returns a Task that will produce the same value is this, but
   * running this resulting task will submit the work to the given
   * ExecutorService
   */
  def fork(implicit pool: ExecutorService): Task[A] = async { cb =>
    val _ = pool.submit(new Callable[Unit] {
      def call = cb(Task.this.unsafePerformIO)
    })
  }
}

object Task extends TaskInstances{
  /**
   * Construct a Task which represents an already calculated eager
   * value
   */
  def now[A](a: A): Task[A] = new Task(Eval.now(a))

  /**
   * Construct a Task that when run will produce an A by evaluating
   * the argument. This is an effect capturing constructor which is
   * suitable for wrapping code which is side-effecting
   */
  def later[A](a: => A): Task[A] = new Task(Eval.later(a))

  /**
   * Construct a task from a thunk that will be executed each time the
   * Task is run. This is an effect capturing constructor which is
   * suitable for wrapping code which is side-effecting.
   */
  def always[A](a: () => A): Task[A] = new Task(new Always(a))

  /**
   *  Construct a Task which will represent an asynchronous
   *  computation. The constructor takes a function which will be
   *  invoked when the Task is run, passing the task a callback (A =>
   *  Unit) which will be called when the asyncrounous computation is
   *  complete.
   */
  def async[A](cb: (A => Unit) => Unit): Task[A] = new Task(Eval.always {
    val cdl = new CountDownLatch(1)
    var result: Option[A] = None
    cb((a: A) => {result = Some(a); cdl.countDown})
    cdl.await
    result.get // YOLO
  })
}

trait TaskInstances {
  implicit val taskMonad: Monad[Task] = new Monad[Task] {
    override def pure[A](a: A): Task[A] = Task.now(a)
    override def map[A,B](fa: Task[A])(f: A => B): Task[B] = fa map f
    override def flatMap[A,B](fa: Task[A])(f: A => Task[B]): Task[B] = fa flatMap f
    override def pureEval[A](x: Eval[A]): Task[A] = new Task(x)
  }

  implicit def taskSemigroup[A](implicit A: Semigroup[A]): Semigroup[Task[A]] = new Semigroup[Task[A]] {
    def combine(x: Task[A], y: Task[A]): Task[A] =
      for {
        xx <- x
        yy <- y
      } yield A.combine(xx,yy)
  }
}
