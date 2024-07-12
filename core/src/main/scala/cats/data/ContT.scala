/*
 * Copyright (c) 2015 Typelevel
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package cats
package data

/**
 * This is a continuation transformer based on the ContT in
 * the Haskell package Control.Monad.Cont
 *
 * This is reasonably straight-forward except that to make
 * a tailRecM implementation we leverage the Defer type class to
 * obtain stack-safety.
 */
sealed abstract class ContT[M[_], A, +B] extends Serializable {
  final def run: (B => M[A]) => M[A] = runAndThen
  protected def runAndThen: AndThen[B => M[A], M[A]]

  final def map[C](fn: B => C)(implicit M: Defer[M]): ContT[M, A, C] = {
    // allocate/pattern match once
    val fnAndThen = AndThen(fn)
    ContT { fn2 =>
      val cb = fnAndThen.andThen(fn2)
      M.defer(run(cb))
    }
  }

  /**
   * c.mapCont(f).run(g) == f(c.run(g))
   */
  final def mapCont(fn: M[A] => M[A]): ContT[M, A, B] =
    // Use later here to avoid forcing run
    ContT.later(runAndThen.andThen(fn))

  /**
   * cont.withCont(f).run(cb) == cont.run(f(cb))
   */
  final def withCont[C](fn: (C => M[A]) => B => M[A]): ContT[M, A, C] =
    // lazy to avoid forcing run
    ContT.later(AndThen(fn).andThen(runAndThen))

  final def flatMap[C](fn: B => ContT[M, A, C])(implicit M: Defer[M]): ContT[M, A, C] = {
    // allocate/pattern match once
    val fnAndThen = AndThen(fn)
    ContT[M, A, C] { fn2 =>
      val contRun: ContT[M, A, C] => M[A] = { c =>
        M.defer(c.run(fn2))
      }
      val fn3: B => M[A] = fnAndThen.andThen(contRun)
      M.defer(run(fn3))
    }
  }

  final def eval(implicit M: Applicative[M], D: Defer[M], ev: B <:< A): M[A] = D.defer(run(b => M.pure(ev(b))))
}

object ContT {

  // Note, we only have two instances of ContT in order to be gentle on the JVM JIT
  // which treats classes with more than two subclasses differently

  private case class FromFn[M[_], A, B](runAndThen: AndThen[B => M[A], M[A]]) extends ContT[M, A, B]

  private case class DeferCont[M[_], A, B](next: () => ContT[M, A, B]) extends ContT[M, A, B] {
    @annotation.tailrec
    private def loop(n: () => ContT[M, A, B]): ContT[M, A, B] =
      n() match {
        case DeferCont(n) => loop(n)
        case notDefer     => notDefer
      }

    lazy val runAndThen: AndThen[B => M[A], M[A]] = loop(next).runAndThen
  }

  /**
   * Lift a pure value into `ContT`
   */
  def pure[M[_], A, B](b: B): ContT[M, A, B] =
    apply { cb =>
      cb(b)
    }

  /**
   * Lifts the `M[B]` into an `ContT[M, A, B]`.
   * {{{
   * scala> import cats._, data._,  implicits._
   * scala> val a: EitherT[Eval, String, Int] = 1.pure[EitherT[Eval, String, *]]
   * scala> val c: cats.data.ContT[EitherT[Eval, String, *], Int, Int] = ContT.liftF(a)
   * scala> c.run(EitherT.rightT(_)).value.value
   * res0: Either[String, Int] = Right(1)
   * scala> c.run(_ => EitherT.leftT("a")).value.value
   * res1: Either[String, Int] = Left(a)
   * }}}
   */
  def liftF[M[_], A, B](mb: M[B])(implicit M: FlatMap[M]): ContT[M, A, B] =
    apply(M.flatMap(mb)(_))

  /**
   * Same as [[liftF]], but expressed as a FunctionK for use with mapK
   * {{{
   * scala> import cats._, data._
   * scala> trait Foo[F[_]] { def bar: F[Int] }
   * scala> def mapK[F[_], G[_]](fooF: Foo[F])(f: F ~> G): Foo[G] = new Foo[G] { def bar: G[Int] = f(fooF.bar) }
   * scala> val eitherTFoo = new Foo[EitherT[Eval, String, *]] { def bar = EitherT.rightT(1) }
   * scala> val contTFoo: Foo[ContT[EitherT[Eval, String, *], Int, *]] = mapK(eitherTFoo)(ContT.liftK)
   * scala> contTFoo.bar.run(EitherT.rightT(_)).value.value
   * res0: Either[String, Int] = Right(1)
   * }}}
   */
  def liftK[M[_], B](implicit M: FlatMap[M]): M ~> ContT[M, B, *] =
    new (M ~> ContT[M, B, *]) {
      def apply[A](ma: M[A]): ContT[M, B, A] = ContT.liftF(ma)
    }

  /*
   * Call with current continuation
   *
   * Passes the current continuation to f, meaning we can model short-circuit
   * evaluation eg exception handling
   *
   * {{{
   *   for {
   *     _ <- ContT.callCC( (k: Unit => ContT[IO, Unit, Unit]) =>
   *       ContT.liftF(IO.println("this will print first")) >>
   *         k(()) >>
   *         ContT.liftF(IO.println("this will NOT print as we short-circuit to the continuation"))
   *     )
   *     _ <- ContT.liftF(IO.println("this will print second")])
   *   } yield ()
   *
   * }}}
   */
  def callCC[M[_], A, B, C](f: (B => ContT[M, A, C]) => ContT[M, A, B])(implicit M: Defer[M]): ContT[M, A, B] =
    apply { cb =>
      val cont = f { a =>
        apply(_ => cb(a))
      }
      M.defer(cont.run(cb))
    }

  /**
   * Similar to [[pure]] but evaluation of the argument is deferred.
   *
   * This is useful for building a computation which calls its continuation as the final step.
   * Instead of writing:
   *
   * {{{
   *   ContT.apply { cb =>
   *     val x = foo()
   *     val y = bar(x)
   *     val z = baz(y)
   *     cb(z)
   *   }
   * }}}
   *
   * you can write:
   *
   * {{{
   *   ContT.defer {
   *     val x = foo()
   *     val y = bar(x)
   *     baz(y)
   *   }
   * }}}
   */
  def defer[M[_], A, B](b: => B): ContT[M, A, B] =
    apply { cb =>
      cb(b)
    }

  /**
   * Build a computation that makes use of a callback, also known as a continuation.
   *
   * Example:
   *
   * {{{
   *   ContT.apply { callback =>
   *     for {
   *       a <- doFirstThing()
   *       b <- doSecondThing(a)
   *       c <- callback(b)
   *       d <- doFourthThing(c)
   *     } yield d
   *   }
   * }}}
   */
  def apply[M[_], A, B](fn: (B => M[A]) => M[A]): ContT[M, A, B] =
    FromFn(AndThen(fn))

  /**
   * Similar to [[apply]] but evaluation of the argument is deferred.
   */
  def later[M[_], A, B](fn: => (B => M[A]) => M[A]): ContT[M, A, B] =
    DeferCont(() => FromFn(AndThen(fn)))

  /*
   * Limits the continuation of any inner [[shiftT]]
   */
  def resetT[M[_]: Monad: Defer, A, B](contT: ContT[M, B, B]): ContT[M, A, B] =
    ContT.liftF(contT.eval)

  /*
   * Captures the continuation up to the nearest enclosing [[resetT]] and passes
   * it to f.
   *
   * For example, in the following the continuation captured as k is
   * {{{ _.map(_ + 1) }}}
   * so the evaluation (modulo Eval) is 2 * (5 + 1)
   *
   * {{{
   *   val cont: Cont[Int, Int] = Cont.reset(
   *     Cont.shift((k: Int => Eval[Int]) =>
   *       Cont.liftF[Int, Int](k(5))
   *     ).map(_ + 1)
   *   ).map(_ * 2)
   *
   *   cont.eval.value === 12
   * }}}
   *
   *
   * For an example with IO, consider the evaluation order of this:
   * {{{
   *   for {
   *     _ <- ContT.resetT(
   *            for {
   *              _ <- ContT.liftF(IO.println("1"))
   *              _ <- ContT.shiftT { (k: Unit => IO[Unit]) =>
   *                     for {
   *                       _ <- ContT.liftF(IO.println("2"))
   *                       _ <- ContT.liftF(k(()))
   *                       _ <- ContT.liftF(IO.println("4"))
   *                     }
   *                   }
   *              _ <- ContT.liftF(IO.println("3"))
   *            } yield ()
   *         )
   *     _ <- ContT.liftF(IO.println("5"))
   *   } yield ()
   * }}}
   *
   * The continuation captured by k is {{{ >> ContT.liftF(IO.println("3")) }}}
   * which is why that is evaluated when {{{ k() }}} is invoked and
   * hence why it is printed before 4.
   */
  def shiftT[M[_]: Applicative: Defer, A, B](f: (B => M[A]) => ContT[M, A, A]): ContT[M, A, B] =
    apply(cb => f(cb).eval)

  def tailRecM[M[_], A, B, C](a: A)(fn: A => ContT[M, C, Either[A, B]])(implicit M: Defer[M]): ContT[M, C, B] =
    ContT[M, C, B] { (cb: (B => M[C])) =>
      def go(a: A): M[C] =
        fn(a).run {
          case Left(a)  => M.defer(go(a))
          case Right(b) => M.defer(cb(b))
        }

      go(a)
    }

  implicit def catsDataContTDefer[M[_], B]: Defer[ContT[M, B, *]] =
    new Defer[ContT[M, B, *]] {
      def defer[A](c: => ContT[M, B, A]): ContT[M, B, A] =
        DeferCont(() => c)
    }

  implicit def catsDataContTMonad[M[_]: Defer, A]: Monad[ContT[M, A, *]] =
    new Monad[ContT[M, A, *]] {
      def pure[B](b: B): ContT[M, A, B] =
        ContT.pure(b)

      override def map[B, C](c: ContT[M, A, B])(fn: B => C): ContT[M, A, C] =
        c.map(fn)

      def flatMap[B, C](c: ContT[M, A, B])(fn: B => ContT[M, A, C]): ContT[M, A, C] =
        c.flatMap(fn)

      def tailRecM[B, C](b: B)(fn: B => ContT[M, A, Either[B, C]]): ContT[M, A, C] =
        ContT.tailRecM(b)(fn)
    }
}
