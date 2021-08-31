package cats.data

private[data] trait ScalaVersionSpecificNonEmptyChainImpl {
  def fromSeq[A](as: scala.collection.Seq[A]): Option[NonEmptyChain[A]] =
    if (as.nonEmpty) Option(NonEmptyChainImpl.create(Chain.fromSeq(as))) else None
}
