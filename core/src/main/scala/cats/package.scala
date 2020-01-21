import scala.annotation.tailrec

/**
 * Symbolic aliases for various types are defined here.
 */
package object cats {

  type ~>[F[_], G[_]] = arrow.FunctionK[F, G]

  type ⊥ = Nothing
  type ⊤ = Any

  /** [[cats.InjectK]][F, G] */
  type :<:[F[_], G[_]] = InjectK[F, G]

  /** [[cats.InjectK]][F, G] */
  type :≺:[F[_], G[_]] = InjectK[F, G]

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
  type Id[+A] = A
  type Endo[A] = A => A
  implicit val catsInstancesForId: Bimonad[({ type L[+x] = x })#L]
    with CommutativeMonad[({ type L[+x] = x })#L]
    with Comonad[({ type L[+x] = x })#L]
    with NonEmptyTraverse[({ type L[+x] = x })#L]
    with Distributive[({ type L[+x] = x })#L] =
    new Bimonad[({ type L[+x] = x })#L]
      with CommutativeMonad[({ type L[+x] = x })#L]
      with Comonad[({ type L[+x] = x })#L]
      with NonEmptyTraverse[({ type L[+x] = x })#L]
      with Distributive[({ type L[+x] = x })#L] {
      def pure[A](a: A): A = a
      def extract[A](a: A): A = a
      def flatMap[A, B](a: A)(f: A => B): B = f(a)
      def coflatMap[A, B](a: A)(f: A => B): B = f(a)
      @tailrec def tailRecM[A, B](a: A)(f: A => Either[A, B]): B = f(a) match {
        case Left(a1) => tailRecM(a1)(f)
        case Right(b) => b
      }
      override def distribute[F[_], A, B](fa: F[A])(f: A => B)(implicit F: Functor[F]): Id[F[B]] = F.map(fa)(f)
      override def map[A, B](fa: A)(f: A => B): B = f(fa)
      override def ap[A, B](ff: A => B)(fa: A): B = ff(fa)
      override def flatten[A](ffa: A): A = ffa
      override def map2[A, B, Z](fa: A, fb: B)(f: (A, B) => Z): Z = f(fa, fb)
      override def lift[A, B](f: A => B): A => B = f
      override def imap[A, B](fa: A)(f: A => B)(fi: B => A): B = f(fa)
      def foldLeft[A, B](a: A, b: B)(f: (B, A) => B) = f(b, a)
      def foldRight[A, B](a: A, lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        f(a, lb)
      def nonEmptyTraverse[G[_], A, B](a: A)(f: A => G[B])(implicit G: Apply[G]): G[B] =
        f(a)
      override def foldMap[A, B](fa: Id[A])(f: A => B)(implicit B: Monoid[B]): B = f(fa)
      override def reduce[A](fa: Id[A])(implicit A: Semigroup[A]): A =
        fa
      def reduceLeftTo[A, B](fa: Id[A])(f: A => B)(g: (B, A) => B): B =
        f(fa)
      override def reduceLeft[A](fa: Id[A])(f: (A, A) => A): A =
        fa
      override def reduceLeftToOption[A, B](fa: Id[A])(f: A => B)(g: (B, A) => B): Option[B] =
        Some(f(fa))
      override def reduceRight[A](fa: Id[A])(f: (A, Eval[A]) => Eval[A]): Eval[A] =
        Now(fa)
      def reduceRightTo[A, B](fa: Id[A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B] =
        Now(f(fa))
      override def reduceRightToOption[A, B](fa: Id[A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[Option[B]] =
        Now(Some(f(fa)))
      override def reduceMap[A, B](fa: Id[A])(f: A => B)(implicit B: Semigroup[B]): B = f(fa)
      override def size[A](fa: Id[A]): Long = 1L
      override def get[A](fa: Id[A])(idx: Long): Option[A] =
        if (idx == 0L) Some(fa) else None
      override def isEmpty[A](fa: Id[A]): Boolean = false
    }

  /**
   * Witness for: Id[A] <-> Unit => A
   */
  implicit val catsRepresentableForId: Representable.Aux[({ type L[+x] = x })#L, Unit] =
    new Representable[({ type L[+x] = x })#L] {
      override type Representation = Unit
      override val F: Functor[Id] = Functor[Id]

      override def tabulate[A](f: Unit => A): Id[A] = f(())

      override def index[A](f: Id[A]): Unit => A = (_: Unit) => f
    }

  implicit val catsParallelForId: Parallel.Aux[({ type L[+x] = x })#L, ({ type L[+x] = x })#L] =
    Parallel.identity[({ type L[+x] = x })#L]

  type Eq[A] = cats.kernel.Eq[A]
  type PartialOrder[A] = cats.kernel.PartialOrder[A]
  type Comparison = cats.kernel.Comparison
  type Order[A] = cats.kernel.Order[A]
  type Hash[A] = cats.kernel.Hash[A]
  type Semigroup[A] = cats.kernel.Semigroup[A]
  type Monoid[A] = cats.kernel.Monoid[A]
  type Group[A] = cats.kernel.Group[A]

  val Eq = cats.kernel.Eq
  val PartialOrder = cats.kernel.PartialOrder
  val Order = cats.kernel.Order
  val Comparison = cats.kernel.Comparison
  val Hash = cats.kernel.Hash
  val Semigroup = cats.kernel.Semigroup
  val Monoid = cats.kernel.Monoid
  val Group = cats.kernel.Group
}
