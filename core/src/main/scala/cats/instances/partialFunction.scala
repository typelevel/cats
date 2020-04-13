package cats.instances
import cats.arrow.Profunctor

trait PartialFunctionInstances {
  implicit val catsStdProFunctorForPartialFunction: Profunctor[PartialFunction] = new Profunctor[PartialFunction] {

    /**
     * Contramap on the first type parameter and map on the second type parameter
     *
     * Example:
     * {{{
     * scala> import cats.implicits._
     * scala> val fab: PartialFunction[Double, Double] = { case x => x + 0.3 }
     * scala> val f: Int => Double = x => x.toDouble / 2
     * scala> val g: Double => Double = x => x * 3
     * scala> val h = fab.dimap(f)(g)
     * scala> h(3)
     * res0: Double = 5.4
     * }}}
     */
    override def dimap[A, B, C, D](fab: PartialFunction[A, B])(f: C => A)(g: B => D): PartialFunction[C, D] =
      new PartialFunction[C, D] {
        override def isDefinedAt(c: C): Boolean = fab.isDefinedAt(f(c))
        override def apply(c: C): D = g(fab(f(c)))
      }
  }
}
