package cats
package laws


trait FunctorEmptyLaws[F[_]] {
  implicit def F: FunctorEmpty[F]


  implicit def functor: Functor[F] = F.functor

  def mapFilterComposition[A, B, C](fa: F[A], f: A => Option[B], g: B => Option[C]): IsEq[F[C]] = {
    val lhs: F[C] = F.mapFilter(F.mapFilter(fa)(f))(g)
    val rhs: F[C] = F.mapFilter(fa)(a => f(a).flatMap(g))
    lhs <-> rhs
  }

  def mapFilterMapConsistency[A, B](fa: F[A], f: A => B): IsEq[F[B]] = {
    F.mapFilter(fa)(f andThen (x => Some(x): Option[B])) <-> functor.map(fa)(f)
  }

  def collectConsistentWithMapFilter[A, B](fa: F[A], f: PartialFunction[A, B]): IsEq[F[B]] = {
    F.collect(fa)(f) <-> F.mapFilter(fa)(f.lift)
  }

  def flattenOptionConsistentWithMapFilter[A](fa: F[Option[A]]): IsEq[F[A]] = {
    F.flattenOption(fa) <-> F.mapFilter(fa)(identity)
  }

  def filterConsistentWithMapFilter[A](fa: F[A], f: A => Boolean): IsEq[F[A]] = {
    F.filter(fa)(f) <->
      F.mapFilter(fa)(a => if (f(a)) Some(a) else None)
  }
}

object FunctorEmptyLaws {
  def apply[F[_]](implicit ev: FunctorEmpty[F]): FunctorEmptyLaws[F] =
    new FunctorEmptyLaws[F] { def F: FunctorEmpty[F] = ev }
}
