package cats.evidence

/** Type inequality (copied from shapeless) */
@scala.annotation.implicitAmbiguous("${A} must not be equal to ${B}")
trait NotEq[A, B]

object NotEq {
  implicit def neq[A, B] : A NotEq B = new NotEq[A, B] {}
  implicit def neqAmbig1[A] : A NotEq A = sys.error("Unexpected invocation")
  implicit def neqAmbig2[A] : A NotEq A = sys.error("Unexpected invocation")
}
