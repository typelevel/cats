package cats

trait Alternative[F[_]] extends Applicative[F] with MonoidK[F] {

  def some[G[_], A](fa: F[A])(implicit G: Alternative[G]): F[G[A]] =
    map2(fa, many(fa))((a,b) => G.combine(G.pure(a), b))

  def many[G[_], A](fa: F[A])(implicit G: Alternative[G]): F[G[A]] =
    combine(some(fa), pure(G.empty))
}

object Alternative {
  def apply[F[_]](implicit ev: Alternative[F]): Alternative[F] = ev
}
