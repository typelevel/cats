package cats.evidence

private[evidence] trait AsSupport {

  /**
   * In 2.13 there is a method on ev that makes this safe.
   * But lack of this method does not make the cast unsafe
   * it just makes it not provable without the cast.
   */
  @inline implicit def asFromPredef[A, B](implicit ev: A <:< B): A As B =
    As.refl[A].asInstanceOf[A As B]
}
