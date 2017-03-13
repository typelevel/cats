package cats
package laws

trait MonadWriterLaws[F[_], W] extends MonadLaws[F] {
  implicit override def F: MonadWriter[F, W]

  def monadWriterWriterPure[A](a: A)(implicit W: Monoid[W]): IsEq[F[A]] =
    F.writer((W.empty, a)) <-> F.pure(a)

  def monadWriterTellFusion(x: W, y: W)(implicit W: Monoid[W]): IsEq[F[Unit]] =
    F.flatMap(F.tell(x))(_ => F.tell(y)) <-> F.tell(W.combine(x, y))

  def monadWriterListenPure[A](a: A)(implicit W: Monoid[W]): IsEq[F[(W, A)]] =
    F.listen(F.pure(a)) <-> F.pure((W.empty, a))

  def monadWriterListenWriter[A](aw: (W, A)): IsEq[F[(W, A)]] =
    F.listen(F.writer(aw)) <-> F.map(F.tell(aw._1))(_ => aw)
}

object MonadWriterLaws {
  def apply[F[_], W](implicit FW: MonadWriter[F, W]): MonadWriterLaws[F, W] =
    new MonadWriterLaws[F, W] { def F: MonadWriter[F, W] = FW }
}
