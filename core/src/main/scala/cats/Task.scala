package cats

import data._

sealed trait Task[A] {
  import Task._

  def map[B](f: A => B): Task[B]
  def flatMap[B](f: A => Task[B]): Task[B] = Bind(this, f)

  def attempt: Task[Xor[Throwable,A]]

  def run: A
}

object Task {
  def now[A](a: A): Task[A] = Value(Eval.now(a))
  def later[A](a: => A): Task[A] = Value(Eval.later(a))
  def always[A](a: () => A): Task[A] = Value(new Always(a))
  def async[A](cb: (A => Unit) => Unit): Task[A] = Async(cb)

  private[cats] final case class Value[A](value: Eval[A]) extends Task[A] {
    override def run = value.value
    override def map[B](f: A => B) = Value(value map f)
    override def attempt = Value(value.attempt)
  }
  private[cats] final case class Async[A](complete: (A => Unit) => Unit) extends Task[A] {
    override def run = {
      val cdl = new java.util.concurrent.CountDownLatch(1)
      var result: Option[A] = None
      complete { a =>
        result = Some(a)
        cdl.countDown
      }
      cdl.await
      result.get
    }
    override def map[B](f: A => B): Task[B] = Bind(this, f andThen now)
    override def attempt: Task[Throwable Xor A] = Async[Throwable Xor A]{(cb: (Throwable Xor A) => Unit) =>
      complete(a => cb(Xor.Right(a)))
    }
  }

  private[cats] final case class Bind[B,A](t: Task[B], f: B => Task[A]) extends Task[A] {
    override def run: A = f(t.run).run
    override def map[C](ff: A => C): Task[C] =
      Bind[B,C](t, f andThen (_ map ff))
    override def attempt: Task[Throwable Xor A] =
      Bind[Xor[Throwable,B],Throwable Xor A](t.attempt, {(b: Throwable Xor B) => Traverse[Throwable Xor ?].sequence(b flatMap (bb => Xor.fromTryCatch(f(bb))))})

  }

  implicit val taskInstances: Bimonad[Task] = new Bimonad[Task] {
    override def map[A,B](fa: Task[A])(f: A => B): Task[B] = fa map f
    override def flatMap[A,B](fa: Task[A])(f: A => Task[B]): Task[B] = fa flatMap f
    override def pure[A](a: A): Task[A] = now(a)
    override def extract[A](a: Task[A]): A = a.run
    override def coflatMap[A, B](fa: cats.Task[A])(f: cats.Task[A] => B): cats.Task[B] = now(f(fa))
  }
}
