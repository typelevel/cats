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

import scala.util.{Either, Left, Right}

/**
 * A mix-in for inheriting tailRecM on monads which define a stack-safe flatMap.  This is
 * ''not'' an appropriate trait to use unless you are 100% certain your monad is stack-safe
 * by definition!  If your monad is not stack-safe, then the tailRecM implementation you
 * will inherit will not be sound, and will result in unexpected stack overflows.  This
 * trait is only provided because a large number of monads ''do'' define a stack-safe
 * flatMap, and so this particular implementation was being repeated over and over again.
 *
 * Note, tailRecM being safe and pure implies that the function passed to flatMap
 * is not called immediately on the current stack (since otherwise tailRecM would
 * stack overflow for sufficiently deep recursions). This implies we can implement
 *
 * defer(fa) = unit.flatMap(_ => fa)
 */
trait StackSafeMonad[F[_]] extends Monad[F] with Defer[F] {

  override def tailRecM[A, B](a: A)(f: A => F[Either[A, B]]): F[B] =
    flatMap(f(a)) {
      case Left(a)  => tailRecM(a)(f)
      case Right(b) => pure(b)
    }

  /*
   * This is always safe for a StackSafeMonad.
   * proof: we know flatMap can't blow the stack
   * because if it could, tailRecM would not be safe:
   * if the function was called in the same stack then
   * the depth would diverse on tailRecM(())(_ => pure(Left(())))
   *
   * It may be better to override this for your particular Monad
   */
  def defer[A](fa: => F[A]): F[A] =
    flatMap(unit)(_ => fa)
}

object StackSafeMonad {
  def shiftFunctor[F[_], A, B](fn: A => F[B])(implicit F: Functor[F]): A => F[B] =
    F match {
      case ssm: StackSafeMonad[F] @unchecked => { a => ssm.defer(fn(a)) }
      case _                                 => fn
    }
}
