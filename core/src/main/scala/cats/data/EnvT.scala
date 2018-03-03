package cats
package data

final case class EnvT[F[_], E, A](ask: E, lower: F[A]) {

  def map[B](f: A => B)(implicit F: Functor[F]): EnvT[F, E, B] =
    EnvT[F, E, B](ask, F.map(lower)(f))

  def coflatten(implicit F: CoflatMap[F]): EnvT[F, E, EnvT[F, E, A]] =
    EnvT[F, E, EnvT[F, E, A]](ask, F.coflatMap(lower)(EnvT[F, E, A](ask, _)))

  def coflatMap[B](f: EnvT[F, E, A] => B)(implicit F: CoflatMap[F]): EnvT[F, E, B] =
    coflatten.map(f)

  def extract(implicit F: Comonad[F]): A = F.extract(lower)

  def local[EE](f: E => EE): EnvT[F, EE, A] = EnvT[F, EE, A](f(ask), lower)

}

object EnvT extends EnvTInstances {

}

private[data] sealed trait EnvTInstances extends EnvTInstances1 {
  implicit def catsDataComonadForEnvT[F[_], E](implicit F0: Comonad[F]): Comonad[EnvT[F, E, ?]] =
    new EnvTComonad[F, E] { implicit def F = F0 }
}

private[data] sealed trait EnvTInstances1 extends EnvTInstances2 {
  implicit def catsDataCoflatMapForEnvT[F[_], E](implicit F0: CoflatMap[F]): CoflatMap[EnvT[F, E, ?]] =
    new EnvTCoflatMap[F, E] { implicit def F = F0 }
}

private[data] sealed trait EnvTInstances2 {
  implicit def catsDataFunctorForEnvT[F[_], E](implicit F0: Functor[F]): Functor[EnvT[F, E, ?]] =
    new EnvTFunctor[F, E] { implicit def F = F0 }
}

private[data] sealed trait EnvTFunctor[F[_], E] extends Functor[EnvT[F, E, ?]] {
  implicit def F: Functor[F]

  def map[A, B](fa: EnvT[F, E, A])(f: A => B): EnvT[F, E, B] = fa.map(f)
}

private[data] sealed trait EnvTCoflatMap[F[_], E] extends CoflatMap[EnvT[F, E, ?]] with EnvTFunctor[F, E] {
  implicit def F: CoflatMap[F]

  override def coflatten[A](fa: EnvT[F, E, A]): EnvT[F, E, EnvT[F, E, A]] = fa.coflatten
  def coflatMap[A, B](fa: EnvT[F, E, A])(f: EnvT[F, E, A] => B): EnvT[F, E, B] = fa.coflatMap(f)
}

private[data] sealed trait EnvTComonad[F[_], E] extends Comonad[EnvT[F, E, ?]] with EnvTCoflatMap[F, E] {
  implicit def F: Comonad[F]

  def extract[A](fa: EnvT[F, E, A]): A = fa.extract
}
