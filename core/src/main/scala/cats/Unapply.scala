package cats

/**
 * A type class that is used to help guide Scala's type inference to
 * find type class instances for types which have shapes which differ
 * from what their type classes are looking for.
 *
 * For example, [[Functor]] is defined for types in the shape
 * F[_]. Scala has no problem finding instance of Functor which match
 * this shape, such as Functor[Option], Functor[List], etc. There is
 * also a functor defined for some types which have the Shape F[_, _]
 * when one of the two 'holes' is fixed. For example. there is a
 * Functor for Map[A, ?] for any A, and for Either[A, ?] for any A,
 * however the Scala compiler will not find them without some coercing.
 */
trait Unapply[TC[_[_]], MA] extends Serializable {
  // a type constructor which is properly kinded for the type class
  type M[_]
  // the type applied to the type constructor to make an MA
  type A

  // the actual type class instance found
  def TC: TC[M]

  // a function which will coerce the MA value into one of type M[A]
  // this will end up being the identity function, but we can't supply
  // it until we have proven that MA and M[A] are the same type
  def subst: MA => M[A]
}

object Unapply extends Unapply2Instances {
  // a convenience method for summoning Unapply instances
  def apply[TC[_[_]], MA](implicit ev: Unapply[TC, MA]): Unapply[TC, MA] = ev

  // the type we will instantiate when we find a type class instance
  // which is already the expected shape: F[_]
  type Aux1[TC[_[_]], MA, F[_], AA] = Unapply[TC, MA] {
    type M[X] = F[X]
    type A = AA
  }

  implicit def catsUnapply1[TC[_[_]], F[_], AA](implicit tc: TC[F]): Aux1[TC, F[AA], F, AA] =
    new Unapply[TC, F[AA]] {
      type M[X] = F[X]
      type A = AA
      override def TC: TC[F] = tc
      override def subst: F[AA] => M[A] = identity
  }
}

private[cats] sealed abstract class Unapply2Instances extends Unapply3Instances {

  // the type we will instantiate when we find a type class instance
  // for a type in the shape F[_, _] when we fix the left type
  type Aux2Left[TC[_[_]], FA, F[_, _], AA, B] = Unapply[TC, FA] {
    type M[X] = F[X, B]
    type A = AA
  }

  // the type we will instantiate when we find a type class instance
  // for a type in the shape F[_, _] when we fix the right type
  type Aux2Right[TC[_[_]], MA, F[_, _], AA, B] = Unapply[TC, MA] {
    type M[X] = F[AA, X]
    type A = B
  }

  // the type we will instantiate when we find a type class instance
  // for a type in the shape F[_, _[_]] when we fix the left type
  type Aux2LeftK[TC[_[_]], FA, F[_, _[_]], AA, BX[_]] = Unapply[TC, FA] {
    type M[X] = F[X, BX]
    type A = AA
  }

  // the type we will instantiate when we find a type class instance
  // for a type in the shape F[_[_], _] when we fix the right type
  type Aux2RightK[TC[_[_]], MA, F[_[_], _], AX[_], B] = Unapply[TC, MA] {
    type M[X] = F[AX, X]
    type A = B
  }


  implicit def catsUnapply2left[TC[_[_]], F[_, _], AA, B](implicit tc: TC[F[?, B]]): Aux2Left[TC, F[AA, B], F, AA, B] = new Unapply[TC, F[AA, B]] {
    type M[X] = F[X, B]
    type A = AA
    def TC: TC[F[?, B]] = tc
    def subst: F[AA, B] => M[A] = identity
  }

  implicit def catsUnapply2right[TC[_[_]], F[_, _], AA, B](implicit tc: TC[F[AA, ?]]): Aux2Right[TC, F[AA, B], F, AA, B] = new Unapply[TC, F[AA, B]] {
    type M[X] = F[AA, X]
    type A = B
    def TC: TC[F[AA, ?]] = tc
    def subst: F[AA, B] => M[A] = identity
  }


  implicit def catsUnapply2leftK[TC[_[_]], F[_, _[_]], AA, B[_]](implicit tc: TC[F[?, B]]): Aux2LeftK[TC, F[AA, B], F, AA, B] = new Unapply[TC, F[AA, B]] {
    type M[X] = F[X, B]
    type A = AA
    def TC: TC[F[?, B]] = tc
    def subst: F[AA, B] => M[A] = identity
  }

  implicit def catsUnapply2rightK[TC[_[_]], F[_[_], _], AA[_], B](implicit tc: TC[F[AA, ?]]): Aux2RightK[TC, F[AA, B], F, AA, B] = new Unapply[TC, F[AA, B]] {
    type M[X] = F[AA, X]
    type A = B
    def TC: TC[F[AA, ?]] = tc
    def subst: F[AA, B] => M[A] = identity
  }



  // STEW: I'm not sure why these Nothing cases are needed and aren't
  // just caught by the generic cases, I'd love for someone to figure
  // that out and report back.
  implicit def catsUnapply2leftN[TC[_[_]], F[_, +_], AA](implicit tc: TC[F[?, Nothing]]): Aux2Left[TC, F[AA, Nothing], F, AA, Nothing] = new Unapply[TC, F[AA, Nothing]] {
    type M[X] = F[X, Nothing]
    type A = AA
    def TC: TC[F[?, Nothing]] = tc
    def subst: F[AA, Nothing] => M[A] = identity
  }

  implicit def catsUnapply2rightN[TC[_[_]], F[+_, _], B](implicit tc: TC[F[Nothing, ?]]): Aux2Right[TC, F[Nothing, B], F, Nothing, B] = new Unapply[TC, F[Nothing, B]] {
    type M[X] = F[Nothing, X]
    type A = B
    def TC: TC[F[Nothing, ?]] = tc
    def subst: F[Nothing, B] => M[A] = identity
  }

  // the type we will instantiate when we find a type class instance
  // for a type in the shape of a Monad Transformer with 2 type params
  type Aux2MT[TC[_[_]], MA, F[_[_], _], AA[_], B] = Unapply[TC, MA] {
    type M[X] = F[AA, X]
    type A = B
  }
}

private[cats] sealed abstract class Unapply3Instances {

  // the type we will instantiate when we find a type class instance
  // for a type in the shape of a Monad Transformer with 3 type params
  // F[_[_], _, _] when we fix the middle type
  type Aux3MTLeft[TC[_[_]], MA, F[_[_], _, _], AA[_], B, C] = Unapply[TC, MA] {
    type M[X] = F[AA, X, C]
    type A = B
  }

  // the type we will instantiate when we find a type class instance
  // for a type in the shape of a Monad Transformer with 3 type params
  // F[_[_], _, _] when we fix the right type
  type Aux3MTRight[TC[_[_]], MA, F[_[_], _, _], AA[_], B, C] = Unapply[TC, MA] {
    type M[X] = F[AA, B, X]
    type A = C
  }


  implicit def catsUnapply3MTLeft[TC[_[_]], F[_[_], _, _], AA[_], B, C](implicit tc: TC[F[AA, ?, C]]): Aux3MTLeft[TC, F[AA, B, C], F, AA, B, C] = new Unapply[TC, F[AA, B, C]] {
    type M[X] = F[AA, X, C]
    type A = B
    def TC: TC[F[AA, ?, C]] = tc
    def subst: F[AA, B, C] => M[A] = identity
  }

  implicit def catsUnapply3MTright[TC[_[_]], F[_[_], _, _], AA[_], B, C](implicit tc: TC[F[AA, B, ?]]): Aux3MTRight[TC, F[AA, B, C], F, AA, B, C] = new Unapply[TC, F[AA, B, C]] {
    type M[X] = F[AA, B, X]
    type A = C
    def TC: TC[F[AA, B, ?]] = tc
    def subst: F[AA, B, C] => M[A] = identity
  }

  type Aux3Nested[TC[_[_]], MA, F[_[_], _[_], _], AA[_], BB[_], C] = Unapply[TC, MA] {
    type M[X] = F[AA, BB, X]
    type A = C
  }

  implicit def catsUnapply3Nested[TC[_[_]], MA, F[_[_], _[_], _], AA[_], BB[_], C](implicit tc: TC[F[AA, BB, ?]]): Aux3Nested[TC, F[AA, BB, C], F, AA, BB, C] = new Unapply[TC, F[AA, BB, C]] {
    type M[X] = F[AA, BB, X]
    type A = C
    def TC: TC[F[AA, BB, ?]] = tc
    def subst: F[AA, BB, C] => M[C] = identity
  }
}
