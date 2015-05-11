/**
 * Symbolic aliases for various types are defined here.
 */
package object cats {

  type ~>[F[_], G[_]] = arrow.NaturalTransformation[F, G]
  type <~[F[_], G[_]] = arrow.NaturalTransformation[G, F]

  type ⊥ = Nothing
  type ⊤ = Any


/**
 * Identity, encoded as `type Id[A] = A`, a convenient alias to make
 * identity instances well-kinded.
 *
 * The identity monad can be seen as the ambient monad that encodes
 * the effect of having no effect. It is ambient in the sense that
 * plain pure values are values of `Id`.
 *
 * For instance, the [[cats.Functor]] instance for `[[cats.Id]]`
 * allows us to apply a function `A => B` to an `Id[A]` and get an
 * `Id[B]`. However, an `Id[A]` is the same as `A`, so all we're doing
 * is applying a pure function of type `A => B` to a pure value  of
 * type `A` to get a pure value of type `B`. That is, the instance
 * encodes pure unary function application.
 */
  type Id[A] = A
  implicit val Id: Bimonad[Id] =
    new Bimonad[Id] {
      def pure[A](a: A): A = a
      def extract[A](a: A): A = a
      def flatMap[A, B](a: A)(f: A => B): B = f(a)
      def coflatMap[A, B](a: A)(f: A => B): B = f(a)
      override def map[A, B](fa: A)(f: A => B): B = f(fa)
      override def ap[A, B](fa: A)(ff: A => B): B = ff(fa)
      override def flatten[A](ffa: A): A = ffa
      override def map2[A, B, Z](fa: A, fb: B)(f: (A, B) => Z): Z = f(fa, fb)
      override def lift[A, B](f: A => B): A => B = f
      override def imap[A, B](fa: A)(f: A => B)(fi: B => A): B = f(fa)
  }

  type Eq[A] = algebra.Eq[A]
  type PartialOrder[A] = algebra.PartialOrder[A]
  type Order[A] = algebra.Order[A]
  type Semigroup[A] = algebra.Semigroup[A]
  type Monoid[A] = algebra.Monoid[A]

  val Eq = algebra.Eq
  val PartialOrder = algebra.PartialOrder
  val Order = algebra.Order
  val Semigroup = algebra.Semigroup
  val Monoid = algebra.Monoid
}
