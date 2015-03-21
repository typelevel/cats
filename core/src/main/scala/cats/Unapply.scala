package cats

trait Unapply[TC[_[_]], MA] {
  type M[_]
  type A

  def TC: TC[M]
  def subst: MA => M[A]
}

object Unapply {
  def apply[TC[_[_]], MA](implicit ev: Unapply[TC,MA]): Unapply[TC, MA] = implicitly

  type Aux1[TC[_[_]], MA, F[_], AA] = Unapply[TC, MA] {
    type M[X] = F[X]
    type A = AA
  }

  type Aux2Left[TC[_[_]], FA, F[_,_], AA, B] = Unapply[TC, FA] {
    type M[X] = F[X,B]
    type A = AA
  }

  type Aux2Right[TC[_[_]], MA, F[_,_], AA, B] = Unapply[TC, MA] {
    type M[X] = F[AA,X]
    type A = B
  }

  implicit def unapply1[TC[_[_]], F[_], AA](implicit tc: TC[F]): Aux1[TC,F[AA],F,AA] =
    new Unapply[TC,F[AA]] {
      type M[X] = F[X]
      type A = AA
      override def TC: TC[F] = tc
      override def subst = identity
  }

   implicit def unapply2left[TC[_[_]], F[_,_], AA, B](implicit tc: TC[F[?,B]]): Aux2Left[TC,F[AA,B], F, AA, B] = new Unapply[TC, F[AA,B]] {
     type M[X] = F[X, B]
     type A = AA
     def TC: TC[F[?, B]] = tc
     def subst = identity
   }
 
   implicit def unapply2leftN[TC[_[_]], F[_,+_], AA](implicit tc: TC[F[?,Nothing]]): Aux2Left[TC,F[AA,Nothing], F, AA, Nothing] = new Unapply[TC, F[AA,Nothing]] {
     type M[X] = F[X, Nothing]
     type A = AA
     def TC: TC[F[?, Nothing]] = tc
     def subst = identity
   }
 
   implicit def unapply2right[TC[_[_]], F[_,_], AA, B](implicit tc: TC[F[AA,?]]): Aux2Right[TC,F[AA,B], F, AA, B] = new Unapply[TC, F[AA,B]] {
     type M[X] = F[AA, X]
     type A = B
     def TC: TC[F[AA, ?]] = tc
     def subst = identity
   }

  implicit def unapply2rightN[TC[_[_]], F[+_,_], B](implicit tc: TC[F[Nothing,?]]): Aux2Right[TC,F[Nothing,B], F, Nothing, B] = new Unapply[TC, F[Nothing,B]] {
     type M[X] = F[Nothing, X]
     type A = B
     def TC: TC[F[Nothing, ?]] = tc
     def subst = identity
   }
}
