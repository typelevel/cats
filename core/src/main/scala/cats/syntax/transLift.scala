package cats
package syntax

trait TransLiftSyntax {
  implicit def transLiftSyntax[M[_], A](ma: M[A]): TransLiftOps[M, A] = new TransLiftOps(ma)
}

final class TransLiftOps[M[_], A](val ma: M[A]) extends AnyVal {
  def liftT[MT[_[_],_]](implicit extract: TLExtract[MT, M]): MT[M, A] = extract.TL.liftT(ma)(extract.TC)
}

trait TLExtract[MT[_[_], _], M[_]] {
  val TL: TransLift[MT]
  val TC: TL.TC[M]
}

object TLExtract {

  implicit def extract1[MT[_[_], _], M[_], TC[_[_]]](implicit TL0: TransLift.Aux[MT, TC], TC0: TC[M]): TLExtract[MT, M] = new TLExtract[MT, M] {
    val TL = TL0
    val TC = TC0
  }

  implicit def extract1Id[MT[_[_], _], M[_]](implicit TL0: TransLift.Aux[MT, Trivial.PH1], TC0: Trivial): TLExtract[MT, M] = extract1[MT, M, Trivial.PH1]

  // sigh...
  implicit def extract2[MT[_[_], _, _], Z, M[_], TC[_[_]]](implicit TL0: TransLift.Aux[MT[?[_], Z, ?], TC], TC0: TC[M]): TLExtract[MT[?[_], Z, ?], M] = extract1[MT[?[_], Z, ?], M, TC]
  implicit def extract2Id[MT[_[_], _, _], Z, M[_]](implicit TL0: TransLift.Aux[MT[?[_], Z, ?], Trivial.PH1], TC0: Trivial): TLExtract[MT[?[_], Z, ?], M] = extract1[MT[?[_], Z, ?], M, Trivial.PH1]
}