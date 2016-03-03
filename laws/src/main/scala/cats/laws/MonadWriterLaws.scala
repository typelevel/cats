package cats
package laws

trait MonadWriterLaws[F[_], W] extends MonadLaws[F] {
  implicit override def F: MonadWriter[F, W]

  def monadWriterWriterPure[A](a: A)(implicit W: Monoid[W]): IsEq[F[A]] =
    F.writer((a, W.empty)) <-> F.pure(a)

  def monadWriterTellFusion(x: W, y: W)(implicit W: Monoid[W]): IsEq[F[Unit]] =
    F.flatMap(F.tell(x))(_ => F.tell(y)) <-> F.tell(W.combine(x, y))

  def monadWriterListenPure[A](a: A)(implicit W: Monoid[W]): IsEq[F[(A, W)]] =
    F.listen(F.pure(a)) <-> F.pure((a, W.empty))

  def monadWriterListenWriter[A](aw: (A, W)): IsEq[F[(A, W)]] =
    F.listen(F.writer(aw)) <-> F.map(F.tell(aw._2))(_ => aw)
}

object MonadWriterLaws {
  def apply[F[_], W](implicit FW: MonadWriter[F, W]): MonadWriterLaws[F, W] =
    new MonadWriterLaws[F, W] { def F: MonadWriter[F, W] = FW }
}
