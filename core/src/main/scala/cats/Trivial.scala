package cats

/**
 * The "Unit type class".  The only instance of `Trivial` is given by
 * `Trivial.manifest`, and this instance is guaranteed to be in the
 * implicit scope.  Several convenience type aliases are provided in
 * companion object, covering a few common use cases and avoiding the
 * need for unnecessary lambdas (e.g. if you want a trivial type class
 * instance for a type constructor, you should use `Trivial.PH1`).
 */
sealed trait Trivial

object Trivial {
  type P1[A] = Trivial
  type PH1[F[_]] = Trivial
  type P1H1[F[_], A] = Trivial
  type P2[A, B] = Trivial
  type P2H1[F[_], A, B] = Trivial
  type P3[A, B, C] = Trivial
  type P3H1[F[_], A, B, C] = Trivial

  implicit val catsTrivialInstance: Trivial = new Trivial {}
}
