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

package algebra

import org.scalacheck.*
import org.scalacheck.util.Pretty
import Prop.{False, Proof, Result}

package object laws {

  lazy val proved = Prop(Result(status = Proof))

  lazy val falsified = Prop(Result(status = False))

  object Ops {
    def run[A](sym: String)(lhs: A, rhs: A)(f: (A, A) => Boolean): Prop =
      if (f(lhs, rhs)) proved
      else
        falsified :| {
          val exp = Pretty.pretty(lhs, Pretty.Params(0))
          val got = Pretty.pretty(rhs, Pretty.Params(0))
          s"($exp $sym $got) failed"
        }
  }

  implicit class CheckEqOps[A](lhs: A)(implicit ev: Eq[A], pp: A => Pretty) {
    def ?==(rhs: A): Prop = Ops.run("?==")(lhs, rhs)(ev.eqv)
    def ?!=(rhs: A): Prop = Ops.run("?!=")(lhs, rhs)(ev.neqv)
  }

  implicit class CheckOrderOps[A](lhs: A)(implicit ev: PartialOrder[A], pp: A => Pretty) {
    def ?<(rhs: A): Prop = Ops.run("?<")(lhs, rhs)(ev.lt)
    def ?<=(rhs: A): Prop = Ops.run("?<=")(lhs, rhs)(ev.lteqv)
    def ?>(rhs: A): Prop = Ops.run("?>")(lhs, rhs)(ev.gt)
    def ?>=(rhs: A): Prop = Ops.run("?>=")(lhs, rhs)(ev.gteqv)
  }

  implicit class BooleanOps[A](lhs: Boolean)(implicit pp: Boolean => Pretty) {
    def ?&&(rhs: Boolean): Prop = Ops.run("?&&")(lhs, rhs)(_ && _)
    def ?||(rhs: Boolean): Prop = Ops.run("?||")(lhs, rhs)(_ || _)
  }
}
