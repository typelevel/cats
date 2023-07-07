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
package instances

import scala.annotation.tailrec

trait HashInstances extends kernel.instances.HashInstances {

  implicit val catsContravariantForHash: Contravariant[Hash] =
    new Contravariant[Hash] {

      /**
       * Derive a `Hash` for `B` given an `Hash[A]` and a function `B => A`.
       */
      def contramap[A, B](ha: Hash[A])(f: B => A): Hash[B] = Hash.by(f)(ha)

    }

  implicit def catsDeferForHash: Defer[Hash] = HashInstances.catsDeferForHashCache
}
object HashInstances {
  private val catsDeferForHashCache: Defer[Hash] =
    new Defer[Hash] {
      case class Deferred[A](fa: () => Hash[A]) extends Hash[A] {
        private lazy val resolve: Hash[A] = {
          @tailrec
          def loop(f: () => Hash[A]): Hash[A] =
            f() match {
              case Deferred(f) => loop(f)
              case next        => next
            }

          loop(fa)
        }

        override def hash(x: A): Int = resolve.hash(x)

        /**
         * Returns `true` if `x` and `y` are equivalent, `false` otherwise.
         */
        override def eqv(x: A, y: A): Boolean = resolve.eqv(x, y)
      }

      override def defer[A](fa: => Hash[A]): Hash[A] = {
        lazy val cachedFa = fa
        Deferred(() => cachedFa)
      }
    }
}
