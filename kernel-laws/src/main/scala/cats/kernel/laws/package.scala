package cats.kernel

import org.scalacheck._
import org.scalacheck.util.Pretty
import Prop.{False, Proof, Result}

package object laws {

  lazy val proved = Prop(Result(status = Proof))

  lazy val falsified = Prop(Result(status = False))

  object Ops {
    def run[A](sym: String)(lhs: A, rhs: A)(f: (A, A) => Boolean): Prop =
      if (f(lhs, rhs)) proved else falsified :| {
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
