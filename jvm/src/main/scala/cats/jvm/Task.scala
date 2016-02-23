package cats
package jvm

import data.Xor
import scala.reflect.ClassTag
import java.util.concurrent.{Callable, CountDownLatch, ExecutorService, Executors, ThreadFactory}

/**
 * A Task is an abstraction of a computation which produces an A
 * possibly asynchronously
 */
sealed trait Task[A] { self =>
  import Task._

  def map[B](f: A => B): Task[B] = new Task[B] {
    def eval = self.eval.map(f)
  }

  def flatMap[B](f: A => Task[B]): Task[B] =
    new Bind(this, f)

  /**
   * Returns a new Task which will evaluate the computation in this
   * Task, but catch any non-fatal exception thrown by the running of
   * the computation.
   */
  def catchNonFatal: Task[Throwable Xor A] =
    new AttemptNonFatal(this)

  /**
   * Returns a new Task which will evaluate the computation in this
   * Task, but catch some subset of the exceptions that might be
   * thrown by the running of the computation.
   */
  def catchOnly[T >: Null <: Throwable: ClassTag]: Task[T Xor A] =
    new AttemptOnly[T, A](this)


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

  protected def eval: Eval[A]
}

object Task extends TaskInstances{

  /**
   * Thread factory to mark all threads as daemon
   */
  lazy val DaemonThreadFactory = new ThreadFactory {
    val defaultThreadFactory = Executors.defaultThreadFactory()
    def newThread(r: Runnable) = {
      val t = defaultThreadFactory.newThread(r)
      t.setDaemon(true)
      t
    }
  }

  /**
   * The default executor service is a fixed thread pool with N daemon threads,
   * where N is equal to the twice number of available processors.
   */
  def createFixedExecutor(numThreads: Int): ExecutorService = {
    Executors.newFixedThreadPool(numThreads, DaemonThreadFactory)
  }


  /**
   * Construct a Task which represents an already calculated eager
   * value
   */
  def now[A](a: A): Task[A] = Value(Eval.now(a))

  /**
   * Construct a Task that when run will produce an A by evaluating
   * the argument. This is an effect capturing constructor which is
   * suitable for wrapping code which is side-effecting
   */
  def later[A](a: => A): Task[A] = Value(Eval.later(a))

  /**
   * Construct a task from a thunk that will be executed each time the
   * Task is run. This is an effect capturing constructor which is
   * suitable for wrapping code which is side-effecting.
   */
  def always[A](a: () => A): Task[A] = Value(new Always(a))

  /**
   *  Construct a Task which will represent an asynchronous
   *  computation. The constructor takes a function which will be
   *  invoked when the Task is run, passing the task a callback (A =>
   *  Unit) which will be called when the asyncrounous computation is
   *  complete.
   */
  def async[A](cb: (A => Unit) => Unit): Task[A] = Async(cb)

  // Here we already have an eval, so we just have to return it
  private[cats] final case class Value[A](eval: Eval[A]) extends Task[A] {
    override def map[B](f: A => B): Task[B] = Value(eval.map(f))
  }

  // Store a flatMap operation on the Heap. when asked to eval, we
  // flatMap against the given task, using Eval.defer to ensure that
  // the computation is stack-safe
  private[cats] final class Bind[Z, A](t: Task[Z], f: Z => Task[A]) extends Task[A] {
    override def eval: Eval[A] = Eval.defer(t.eval.flatMap(f andThen (_.eval)))
  }

  /**
   * Store the intent to catch non-fatal exceptions when running a Task.
   */
  private[cats] final case class AttemptNonFatal[A](task: Task[A]) extends Task[Xor[Throwable, A]] {
    override def eval: Eval[Xor[Throwable, A]] =
      Eval.always(Xor.catchNonFatal(task.eval.value))
  }

  /**
   * Store the intent to catching some subset of the possible
   * excpetions when running a Task.
   */
  private[cats] final case class AttemptOnly[T >: Null <: Throwable : ClassTag, A](task: Task[A]) extends Task[T Xor A] {
    override def eval: Eval[Xor[T, A]] =
      Eval.always(Xor.catchOnly[T](task.eval.value))
  }

  private[cats] final case class Async[A](complete: (A => Unit) => Unit) extends Task[A] {
    override def eval: Eval[A] = {
      val cdl = new CountDownLatch(1)
      var result: Option[A] = None
      complete((a: A) => {result = Some(a); cdl.countDown})
      cdl.await
      Eval.now(result.get) // YOLO
    }
  }
}

trait TaskInstances {
  implicit val taskInstances: Monad[Task] = new Monad[Task] {
    override def pure[A](a: A): Task[A] = Task.now(a)
    override def map[A,B](fa: Task[A])(f: A => B): Task[B] = fa map f
    override def flatMap[A,B](fa: Task[A])(f: A => Task[B]): Task[B] = fa flatMap f
    override def pureEval[A](x: Eval[A]): Task[A] = Task.Value(x)
  }
}
