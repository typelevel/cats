import scala.annotation.tailrec

/**
 * The `cats` root package contains all the trait signatures of most Scala type classes.
 *
 * Cats type classes are implemented using the approach from the
 *  [[https://ropas.snu.ac.kr/~bruno/papers/TypeClasses.pdf Type classes as objects and implicits]] article.
 *
 * For each type class, `cats` provides three pieces:
 * - Its '''signature''': a trait that is polymorphic on a type parameter.
 *   Type class traits inherit from other type classes to indicate that any implementation of the lower type class (e.g. `Applicative`)
 *   can also serve as an instance for the higuer type class (e.g. `Functor`).
 * - Type class ''''instances''', which are classes and objects that implement one or more type class signatures for some specific types.
 *   Type class instances for several data types from the Java or Scala standard libraries are declared in the subpackage `cats.instances`.
 * - '''Syntax extensions''', each of which provides the methods of the type class defines as extension methods
 *   (which in Scala 2 are encoded as implicit classes) for values of any type `F`; given that an instance of the type class
 *   for the receiver type (`this`) is in the implicit scope.
 *   Symtax extensions are declared in the `cats.syntax` package.
 * - A set of '''laws''', that are also generic on the type of the class, and are only defined on the operations of the type class.
 *   The purpose of these laws is to declare some algebraic relations (equations) between Scala expressions involving the operations
 *   of the type class, and test (but not verify) that implemented instances satisfy those equations.
 *   Laws are defined in the `cats-laws` package.
 *
 * Although most of cats type classes are declared in this package, some are declared in other packages:
 * - type classes that operate on base types (kind `*`), and their implementations for standard library types,
 *   are contained in `cats.kernel`, which is a different SBT project. However, they are re-exported from this package.
 * - type classes of kind `F[_, _]`, such as [[cats.arrow.Profunctor]]" or [[cats.arrow.Arrow]], which are relevant for
 *   Functional Reactive Programming or optics, are declared in the `cats.arrow` package.
 * - Also, those type classes that abstract over (pure or impure) functional runtime effects are declared
 *   in the [[https://typelevel.org/cats-effect/ cats-effect library]].
 * - Some type classes for which no laws can be provided are left out of the main road, in a small and dirty alley.
 *   These are the `alleycats`.
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
  type Id[A] = A
  type Endo[A] = A => A
  implicit val catsInstancesForId
    : Bimonad[Id] with CommutativeMonad[Id] with Comonad[Id] with NonEmptyTraverse[Id] with Distributive[Id] =
    new Bimonad[Id] with CommutativeMonad[Id] with Comonad[Id] with NonEmptyTraverse[Id] with Distributive[Id] {
      def pure[A](a: A): A = a
      def extract[A](a: A): A = a
      def flatMap[A, B](a: A)(f: A => B): B = f(a)
      def coflatMap[A, B](a: A)(f: A => B): B = f(a)
      @tailrec def tailRecM[A, B](a: A)(f: A => Either[A, B]): B =
        f(a) match {
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
  implicit val catsRepresentableForId: Representable.Aux[Id, Unit] = new Representable[Id] {
    override type Representation = Unit
    override val F: Functor[Id] = Functor[Id]

    override def tabulate[A](f: Unit => A): Id[A] = f(())

    override def index[A](f: Id[A]): Unit => A = (_: Unit) => f
  }

  implicit val catsParallelForId: Parallel.Aux[Id, Id] = Parallel.identity

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
