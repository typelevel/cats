package cats
package laws

trait MonadWriterLaws[F[_], W] extends MonadLaws[F] {
  implicit def FW: MonadWriter[F, W]
  implicit def F: Monad[F] = FW.monadInstance

  def monadWriterWriterPure[A](a: A)(implicit W: Monoid[W]): IsEq[F[A]] =
    FW.writer((W.empty, a)) <-> F.pure(a)

  def monadWriterTellFusion(x: W, y: W)(implicit W: Monoid[W]): IsEq[F[Unit]] =
    F.flatMap(FW.tell(x))(_ => FW.tell(y)) <-> FW.tell(W.combine(x, y))

  def monadWriterListenPure[A](a: A)(implicit W: Monoid[W]): IsEq[F[(W, A)]] =
    FW.listen(F.pure(a)) <-> F.pure((W.empty, a))

  def monadWriterListenWriter[A](aw: (W, A)): IsEq[F[(W, A)]] =
    FW.listen(FW.writer(aw)) <-> F.map(FW.tell(aw._1))(_ => aw)
}

object MonadWriterLaws {
  def apply[F[_], W](implicit FW0: MonadWriter[F, W]): MonadWriterLaws[F, W] =
    new MonadWriterLaws[F, W] { def FW: MonadWriter[F, W] = FW0 }
}
