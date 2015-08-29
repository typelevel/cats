package cats

import data._

import java.util.concurrent.CountDownLatch

sealed trait Task[A] {
  import Task._

  def run: A

  def map[B](f: A => B): Task[B] =
    flatMap(a => Task.now(f(a)))
  def flatMap[B](f: A => Task[B]): Task[B] =
    Bind(this, f)
  def attempt: Task[Xor[Throwable, A]] =
    Attempt(this)
}

object Task {
  def now[A](a: A): Task[A] = Value(Eval.now(a))
  def later[A](a: => A): Task[A] = Value(Eval.later(a))
  def always[A](a: () => A): Task[A] = Value(new Always(a))
  def async[A](cb: (A => Unit) => Unit): Task[A] = Async(cb)

  private[cats] final case class Value[A](value: Eval[A]) extends Task[A] {
    def run: A =
      value.value
    override def map[B](f: A => B): Task[B] =
      Value(value.map(f))
  }

  private[cats] final case class Attempt[A](task: Task[A]) extends Task[Xor[Throwable, A]] {
    override def run: Xor[Throwable, A] = Xor.catching(task.run)
  }

  private[cats] final case class Async[A](complete: (A => Unit) => Unit) extends Task[A] {
    override def run: A = {
      val cdl = new CountDownLatch(1)
      var result: Option[A] = None
      complete({ (a: A) => result = Some(a); cdl.countDown })
      cdl.await
      result.get
    }
  }

  private[cats] final case class Bind[Z, A](t: Task[Z], f: Z => Task[A]) extends Task[A] {
    override def run: A = f(t.run).run
  }

  implicit val taskInstances: Bimonad[Task] = new Bimonad[Task] {
    override def map[A,B](fa: Task[A])(f: A => B): Task[B] = fa map f
    override def flatMap[A,B](fa: Task[A])(f: A => Task[B]): Task[B] = fa flatMap f
    override def pure[A](a: A): Task[A] = now(a)
    override def extract[A](a: Task[A]): A = a.run
    override def coflatMap[A, B](fa: cats.Task[A])(f: cats.Task[A] => B): cats.Task[B] = now(f(fa))
  }
}
