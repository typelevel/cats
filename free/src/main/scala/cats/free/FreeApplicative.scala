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
package free

import cats.arrow.FunctionK
import cats.data.Const

import scala.annotation.tailrec

/**
 * Applicative Functor for Free,
 * implementation inspired by https://github.com/safareli/free/pull/31/
 */
sealed abstract class FreeApplicative[F[_], A] extends Product with Serializable {
  self =>

  import FreeApplicative.{lift, Ap, FA, Pure}

  final def ap[B](b: FA[F, A => B]): FA[F, B] =
    b match {
      case Pure(f) =>
        this.map(f)
      case _ =>
        Ap(b, this)
    }

  final def map[B](f: A => B): FA[F, B] =
    this match {
      case Pure(a) => Pure(f(a))
      case _       => Ap(Pure(f), this)
    }

  final def map2[B, C](fb: FA[F, B])(f: (A, B) => C): FA[F, C] =
    this match {
      case Pure(a) => fb.map(f(a, _))
      case _ =>
        fb match {
          case Pure(b) => Ap(Pure(f(_: A, b)), this)
          case _       => Ap(Ap(Pure((a: A) => (b: B) => f(a, b)), this), fb)
        }
    }

  /**
   * Interprets/Runs the sequence of operations using the semantics of `Applicative` G[_].
   * Tail recursive.
   */
  final def foldMap[G[_]](f: F ~> G)(implicit G: Applicative[G]): G[A] = {
    import FreeApplicative._
    // the remaining arguments to G[A => B]'s
    var argsF: List[FA[F, Any]] = this.asInstanceOf[FA[F, Any]] :: Nil
    var argsFLength: Int = 1
    // the remaining stack of G[A => B]'s to be applied to the arguments
    var fns: List[Fn[G, Any, Any]] = Nil
    var fnsLength: Int = 0

    @tailrec
    def loop(): G[Any] = {
      var argF: FA[F, Any] = argsF.head
      argsF = argsF.tail
      argsFLength -= 1

      // rip off every `Ap` in `argF` in function position
      if (argF.isInstanceOf[Ap[F, ?, ?]]) {
        val lengthInitial = argsFLength
        // reassociate the functions into a single fn,
        // and move the arguments into argsF
        while ({
          val ap = argF.asInstanceOf[Ap[F, Any, Any]]
          argsF ::= ap.fp
          argsFLength += 1
          argF = ap.fn.asInstanceOf[FA[F, Any]]
          argF.isInstanceOf[Ap[F, ?, ?]]
        }) ()
        // consecutive `ap` calls have been queued as operations;
        // argF is no longer an `Ap` node, so the entire topmost left-associated
        // function application branch has been looped through and we've
        // moved (`argsFLength` - `lengthInitial`) arguments to the stack, through
        // (`argsFLength` - `lengthInitial`) `Ap` nodes, so the function on the right
        // which consumes them all must have (`argsFLength` - `lengthInitial`) arguments
        val argc = argsFLength - lengthInitial
        fns ::= Fn[G, Any, Any](foldArg(argF.asInstanceOf[FA[F, Any => Any]], f), argc)
        fnsLength += 1
        loop()
      } else {
        val argT: G[Any] = foldArg(argF, f)
        if (fns ne Nil) {
          // single right-associated function application
          var fn = fns.head
          fns = fns.tail
          fnsLength -= 1
          var res = G.ap(fn.gab)(argT)
          if (fn.argc > 1) {
            // this function has more than 1 argument,
            // bail out of nested right-associated function application
            fns ::= Fn(res.asInstanceOf[G[Any => Any]], fn.argc - 1)
            fnsLength += 1
            loop()
          } else {
            if (fnsLength > 0) {
              // we've got a nested right-associated `Ap` tree,
              // so apply as many functions as possible
              @tailrec
              def innerLoop(): Unit = {
                fn = fns.head
                fns = fns.tail
                fnsLength -= 1
                res = G.ap(fn.gab)(res)
                if (fn.argc > 1) {
                  fns ::= Fn(res.asInstanceOf[G[Any => Any]], fn.argc - 1)
                  fnsLength += 1
                }
                // we have to bail out if fn has more than one argument,
                // because it means we may have more left-associated trees
                // deeper to the right in the application tree
                if (fn.argc == 1 && fnsLength > 0) innerLoop()
              }

              innerLoop()
            }
            if (fnsLength == 0) res
            else loop()
          }
        } else argT
      }
    }

    loop().asInstanceOf[G[A]]
  }

  /**
   * Interpret/run the operations using the semantics of `Applicative[F]`.
   * Stack-safe.
   */
  final def fold(implicit F: Applicative[F]): F[A] =
    foldMap(FunctionK.id[F])

  /**
   * Interpret this algebra into another algebra.
   * Stack-safe.
   */
  final def compile[G[_]](f: F ~> G): FA[G, A] =
    foldMap[FA[G, *]] {
      new FunctionK[F, FA[G, *]] { def apply[B](fb: F[B]): FA[G, B] = lift(f(fb)) }
    }

  /**
   * Interpret this algebra into a FreeApplicative over another algebra.
   * Stack-safe.
   */
  def flatCompile[G[_]](f: F ~> FA[G, *]): FA[G, A] =
    foldMap(f)

  /**
   * Interpret this algebra into a Monoid.
   */
  final def analyze[M: Monoid](f: FunctionK[F, λ[α => M]]): M =
    foldMap[Const[M, *]](
      new FunctionK[F, Const[M, *]] { def apply[B](fb: F[B]): Const[M, B] = Const(f(fb)) }
    ).getConst

  /**
   * Compile this FreeApplicative algebra into a Free algebra.
   */
  final def monad: Free[F, A] =
    foldMap[Free[F, *]] {
      new FunctionK[F, Free[F, *]] { def apply[B](fb: F[B]): Free[F, B] = Free.liftF(fb) }
    }

  override def toString: String = "FreeApplicative(...)"
}

object FreeApplicative {
  type FA[F[_], A] = FreeApplicative[F, A]

  // Internal helper function for foldMap, it folds only Pure and Lift nodes
  private[free] def foldArg[F[_], G[_], A](node: FA[F, A], f: F ~> G)(implicit G: Applicative[G]): G[A] =
    node match {
      case Pure(x)  => G.pure(x)
      case Lift(fa) => f(fa)
      case x        => throw new RuntimeException(s"Impossible for a $x to reach here")
    }

  /**
   * Represents a curried function `F[A => B => C => ...]`
   * that has been constructed with chained `ap` calls.
   * Fn#argc denotes the amount of curried params remaining.
   */
  final private case class Fn[G[_], A, B](gab: G[A => B], argc: Int)

  final private case class Pure[F[_], A](a: A) extends FA[F, A]

  final private case class Lift[F[_], A](fa: F[A]) extends FA[F, A]

  final private case class Ap[F[_], P, A](fn: FA[F, P => A], fp: FA[F, P]) extends FA[F, A]

  final def pure[F[_], A](a: A): FA[F, A] =
    Pure(a)

  final def ap[F[_], P, A](fp: F[P])(f: FA[F, P => A]): FA[F, A] =
    Ap(f, Lift(fp))

  final def lift[F[_], A](fa: F[A]): FA[F, A] =
    Lift(fa)

  implicit final def freeApplicative[S[_]]: Applicative[FA[S, *]] =
    new Applicative[FA[S, *]] {
      override def product[A, B](fa: FA[S, A], fb: FA[S, B]): FA[S, (A, B)] =
        map2(fa, fb)((_, _))

      override def map[A, B](fa: FA[S, A])(f: A => B): FA[S, B] = fa.map(f)

      override def ap[A, B](f: FA[S, A => B])(fa: FA[S, A]): FA[S, B] = fa.ap(f)

      def pure[A](a: A): FA[S, A] = Pure(a)

      override def map2[A, B, Z](fa: FA[S, A], fb: FA[S, B])(f: (A, B) => Z): FA[S, Z] =
        fa.map2(fb)(f)
    }

}
