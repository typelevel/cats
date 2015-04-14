package cats

trait Alternative[F[_]] extends Applicative[F] with MonoidK[F] {

}

object Alternative {
  def apply[F[_]](implicit ev: Alternative[F]): Alternative[F] = ev
}
