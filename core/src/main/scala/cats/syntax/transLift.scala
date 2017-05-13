package cats
package syntax

trait TransLiftSyntax {
  implicit final def catsSyntaxTransLift[M0[_], A](ma: M0[A]): TransLiftOps[M0, A] = new TransLiftOps(ma)
}

final class TransLiftOps[M0[_], A](val ma: M0[A]) extends AnyVal {
  import TLExtract._

  def liftT[MT0[_[_], _]](implicit extract: TLExtract[SingletonMT { type MT[F[_], B] = MT0[F, B] }, SingletonM { type M[B] = M0[B] }]): MT0[M0, A] = extract.TL.liftT(ma)(extract.TC)
}

trait TLExtract[MTS <: TLExtract.SingletonMT, MS <: TLExtract.SingletonM] {
  val TL: TransLift[MTS#MT]
  val TC: TL.TC[MS#M]
}

object TLExtract {

  trait SingletonMT {
    type MT[F[_], A]
  }

  trait SingletonM {
    type M[A]
  }

  implicit def extract[MTS <: SingletonMT, MS <: SingletonM, TC[_[_]]](implicit TL0: TransLift.Aux[MTS#MT, TC], TC0: TC[MS#M]): TLExtract[MTS, MS] = new TLExtract[MTS, MS] {
    val TL = TL0
    val TC = TC0
  }

  implicit def extractId[MTS <: SingletonMT, MS <: SingletonM](implicit TL0: TransLift.Aux[MTS#MT, Trivial.PH1]): TLExtract[MTS, MS] = extract[MTS, MS, Trivial.PH1]
}
