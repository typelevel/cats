package cats.evidence

private[evidence] trait IsSupport {
  @inline implicit def isFromPredef[A, B](implicit ev: A =:= B): A Is B =
    ev.substituteCo[Is[A, *]](Is.refl[A])
}
