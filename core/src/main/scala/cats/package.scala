import scala.annotation.tailrec
import cats.data.Xor

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
  implicit val idInstances: Bimonad[Id] with MonadRec[Id] with Traverse[Id] =
    new Bimonad[Id] with MonadRec[Id] with Traverse[Id] {
      def pure[A](a: A): A = a
      def extract[A](a: A): A = a
      def flatMap[A, B](a: A)(f: A => B): B = f(a)
      def coflatMap[A, B](a: A)(f: A => B): B = f(a)
      @tailrec def tailRecM[A, B](a: A)(f: A => A Xor B): B = f(a) match {
        case Xor.Left(a1) => tailRecM(a1)(f)
        case Xor.Right(b) => b
      }
      override def map[A, B](fa: A)(f: A => B): B = f(fa)
      override def ap[A, B](ff: A => B)(fa: A): B = ff(fa)
      override def flatten[A](ffa: A): A = ffa
      override def map2[A, B, Z](fa: A, fb: B)(f: (A, B) => Z): Z = f(fa, fb)
      override def lift[A, B](f: A => B): A => B = f
      override def imap[A, B](fa: A)(f: A => B)(fi: B => A): B = f(fa)
      def foldLeft[A, B](a: A, b: B)(f: (B, A) => B) = f(b, a)
      def foldRight[A, B](a: A, lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        f(a, lb)
      def traverse[G[_], A, B](a: A)(f: A => G[B])(implicit G: Applicative[G]): G[B] =
        f(a)
  }

  type Eq[A] = cats.kernel.Eq[A]
  type PartialOrder[A] = cats.kernel.PartialOrder[A]
  type Order[A] = cats.kernel.Order[A]
  type Semigroup[A] = cats.kernel.Semigroup[A]
  type Monoid[A] = cats.kernel.Monoid[A]
  type Group[A] = cats.kernel.Group[A]

  val Eq = cats.kernel.Eq
  val PartialOrder = cats.kernel.PartialOrder
  val Order = cats.kernel.Order
  val Semigroup = cats.kernel.Semigroup
  val Monoid = cats.kernel.Monoid
  val Group = cats.kernel.Group
}
