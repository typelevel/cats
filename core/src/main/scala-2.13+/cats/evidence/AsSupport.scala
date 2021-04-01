package cats.evidence

private[evidence] trait AsSupport {
  @inline implicit def asFromPredef[A, B](implicit ev: A <:< B): A As B = {
    // we need F to be covariant, and the type lambda loses that
    // if we write As[A, *]
    type F[+Z] = As[A, Z]
    ev.substituteCo[F](As.refl[A])
  }
}
