package cats
package syntax

trait TransLiftSyntax {
  implicit def transLiftSyntax[M[_], A](ma: M[A]): TransLiftOps[M, A] = new TransLiftOps(ma)
}

final class TransLiftOps[M[_], A](val ma: M[A]) extends AnyVal {
  def liftT[MT[_[_],_]](implicit TL: TransLift[MT, M]): MT[M,A] = TL.liftT(ma)
}
