package cats

import cats.arrow.{ArrowChoice, Category, Strong}
import cats.evidence.=~~=
import Iso.HasIso
import cats.implicits._

trait Iso[->[_, _], A, B] { self =>
  def cat: Category[->]
  def to: A -> B
  def from: B -> A

  final def apply(a: A)(implicit ev: -> =~~= Function1): B = ev(to)(a)

  final def andThen[C](bc: Iso[->, B, C]): Iso[->, A, C] =
    Iso.createUnsafe(cat.andThen(self.to, bc.to), cat.andThen(bc.from, self.from))(cat)

  final def compose[Z](za: Iso[->, Z, A]): Iso[->, Z, B] = za.andThen(self)

  /** From A <-> B, X <-> Y we can obtain (A, X) <-> (B, Y) if -> has a Strong instance */
  def and[X, Y](that: Iso[->, X, Y])(implicit A: Strong[->]): Iso[->, (A, X), (B, Y)] =
    Iso.createUnsafe(
      self.cat.andThen(A.first[A, B, X](self.to), A.second[X, Y, B](that.to)),
      self.cat.andThen(A.first[B, A, Y](self.from), A.second[Y, X, A](that.from))
    )(self.cat)

  /** From A <-> B, X <-> Y we can obtain Either[A, X] <-> Either[B, Y] if -> has an ArrowChoice instance */
  def or[X, Y](that: Iso[->, X, Y])(implicit A: ArrowChoice[->]): Iso[->, Either[A, X], Either[B, Y]] =
    Iso.createUnsafe(A.choose(self.to)(that.to), A.choose(self.from)(that.from))

  /** Flips the isomorphism from A <-> B to B <-> A grace to it's reflexivity property */
  def flip: Iso[->, B, A] = new Iso[->, B, A] {
    val (cat, to, from) = (self.cat, self.from, self.to)
    override val flip = self
  }

  /** If A <-> B then having a function B -> B we can obtain A -> A */
  def teleport(fn: B -> B): A -> A = cat.compose(from, cat.compose(fn, to))

  /** For some invariant F[_] if we have an F[A] we can obtain an F[B] using A <-> B */
  def derive[F[_]](implicit fa: F[A], I: Invariant[F], eq: -> =~~= Function1): F[B] =
    I.imap(fa)(eq(self.to))(eq(self.from))

  /** Having A <-> B searches implicits for B <-> C to obtain A <-> C */
  def chain[C](implicit hi: HasIso[->, B, C]): Iso[->, A, C] = self.andThen(hi)
}

object Iso {
  type <=>[A, B] = Iso[* => *, A, B]

  /** Create an isomorphism from (A -> B, B -> A) as long as you promise to abide the isomorphism laws */
  def createUnsafe[->[_, _], A, B](ab: A -> B, ba: B -> A)(implicit C: Category[->]): Iso[->, A, B] =
    new Iso[->, A, B] { val cat = C; val (to, from) = (ab, ba) }

  /** Isomorphism is reflexive: for any A we have A <-> A */
  def refl[A]: Iso[* => *, A, A] = reflAny.asInstanceOf
  private[this] val reflAny = new Iso[* => *, Any, Any] {
    val (cat, to, from) = (Category[* => *], (a: Any) => a, (a: Any) => a)
  }

  def refl[->[_, _], A](implicit C: Category[->]): Iso[->, A, A] =
    new Iso[->, A, A] { val (cat, to, from) = (C, C.id[A], C.id[A]) }

  object Product {
    final def associate[A, B, C]: (A, (B, C)) <=> ((A, B), C) =
      createUnsafe({ case (a, (b, c)) => ((a, b), c) }, { case ((a, b), c) => (a, (b, c)) })
    final def commute[A, B]: (A, B) <=> (B, A) =
      createUnsafe({ case (a, b)                                                    => (b, a) }, { case (b, a) => (a, b) })
    final def unitL[A]: A <=> (Unit, A) = createUnsafe(a => ((), a), { case ((), a) => a })
    final def unitR[A]: A <=> (A, Unit) = createUnsafe(a => (a, ()), { case (a, ()) => a })
    final def first[A, B, C](iso: A <=> C): (A, B) <=> (C, B) = iso.and(refl[B])
    final def second[A, B, C](iso: B <=> C): (A, B) <=> (A, C) = refl[A].and(iso)
  }

  object Coproduct {
    final def associate[A, B, C]: Either[A, Either[B, C]] <=> Either[Either[A, B], C] =
      createUnsafe(
        _.fold(a => Left(Left(a)), _.fold(b => Left(Right(b)), c => Right(c))),
        _.fold(_.fold(a => Left(a), b => Right(Left(b))), c => Right(Right(c)))
      )
    final def commute[A, B]: Either[A, B] <=> Either[B, A] =
      createUnsafe((e: Either[A, B]) => e.swap, (e: Either[B, A]) => e.swap)
    final def unitL[A]: A <=> Either[Nothing, A] = createUnsafe(a => Right(a), _.fold((n: A) => n, identity))
    final def unitR[A]: A <=> Either[A, Nothing] = createUnsafe(a => Left(a), _.fold(identity, (n: A) => n))
    final def first[A, B, C](iso: A <=> C): Either[A, B] <=> Either[C, B] = iso.or(refl[B])
    final def second[A, B, C](iso: B <=> C): Either[A, B] <=> Either[A, C] = refl[A].or(iso)
  }

  def isoUnitToA[A]: <=>[Unit => A, A] = Iso.createUnsafe(_(()), a => _ => a)
  def isoAToUnit[A]: <=>[A => Unit, Unit] = Iso.createUnsafe(_ => (), _ => _ => ())
  def isoNAToUnit[A]: <=>[Nothing => A, Unit] = Iso.createUnsafe(_ => (), _ => identity[A])

  /** Newtype for Iso[->, A, B]; useful for implicit search, as it works flipped and also reflexively */
  type HasIso[->[_, _], A, B] = HasIso.Type[->, A, B]

  object HasIso {
    type Base
    trait Tag extends Any
    type Type[->[_, _], A, B] <: Base with Tag

    def apply[->[_, _], A, B](i: Iso[->, A, B]): Type[->, A, B] = i.asInstanceOf[Type[->, A, B]]
    def unwrap[->[_, _], A, B](h: Type[->, A, B]): Iso[->, A, B] = h.asInstanceOf[Iso[->, A, B]]

    import inequality.=!=

    implicit def hasIsoReflImp[->[_, _]: Category, A]: HasIso[->, A, A] = HasIso(Iso.refl[->, A])
    implicit def hasIsoNormImp[->[_, _], A: * =!= B, B](implicit I: Iso[->, A, B]): HasIso[->, A, B] = HasIso(I)
    implicit def hasIsoFlipImp[->[_, _], A: * =!= B, B](implicit I: Iso[->, B, A]): HasIso[->, A, B] = HasIso(I.flip)

    implicit def conversionToIso[->[_, _], A, B](hi: HasIso[->, A, B]): Iso[->, A, B] = hi.iso

    implicit class HasIsoOps[->[_, _], A, B](val hi: HasIso[->, A, B]) extends AnyVal {
      def iso: Iso[->, A, B] = HasIso.unwrap(hi)
    }

    private[cats] object inequality {
      trait =!=[A, B]
      implicit def neq[A, B]: A =!= B = null
      @scala.annotation.implicitAmbiguous("Could not prove that ${A} =!= ${A}")
      implicit def neqAmbig1[A]: A =!= A = null
      implicit def neqAmbig2[A]: A =!= A = null
    }
  }

}
