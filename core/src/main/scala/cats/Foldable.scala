package cats

import simulacrum._

/**
 * Data structures that can be folded to a summary value.
 *
 * See: [[https://www.cs.nott.ac.uk/~gmh/fold.pdf A tutorial on the universality and expressiveness of fold]]
 */
@typeclass trait Foldable[F[_]] { self =>

  /**
   * Left associative fold on 'F' using the function 'f'.
   */
  def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B

  /**
   * Right associative lazy fold on `F` using the folding function 'f'.
   *
   * This method evaluates `b` lazily (in some cases it will not be
   * needed), and returns a lazy value. We are using `A => Fold[B]` to
   * support laziness in a stack-safe way.
   *
   * For more detailed information about how this method works see the
   * documentation for `Fold[_]`.
   */
  def foldLazy[A, B](fa: F[A], b: Lazy[B])(f: A => Fold[B]): Lazy[B]

  /**
   * Right associative fold on 'F' using the function 'f'.
   */
  def foldRight[A, B](fa: F[A], b: B)(f: (A, B) => B): B =
    foldLazy(fa, Lazy.eager(b)) { a =>
      Fold.Continue(b => f(a, b))
    }.force

  /**
   * Apply f to each element of F and combine them using the Monoid[B].
   */
  def foldMap[A, B](fa: F[A])(f: A => B)(implicit B: Monoid[B]): B =
    foldLeft(fa, B.empty) { (b, a) =>
      B.combine(b, f(a))
    }

  /**
   * Fold up F using the Monoid[A]
   */
  def fold[A](fa: F[A])(implicit A: Monoid[A]): A =
    foldLeft(fa, A.empty) { (acc, a) =>
      A.combine(acc, a)
    }

  /**
   * Traverse F in the Applicative G and ignore the return values of 'f'.
   */
  def traverse_[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[Unit] =
    foldLeft(fa, Applicative[G].pure(())) { (acc, a) =>
      Applicative[G].map2(acc, f(a)) { (_, _) => () }
    }

  /**
   * Traverse F in the Applicative G ignoring all values in fga.
   */
  def sequence_[G[_]: Applicative, A, B](fga: F[G[A]]): G[Unit] =
    traverse_(fga)(identity)

  /**
   * Fold up F using the MonoidK instance for G. Like fold, but the value is of kind * -> *.
   */
  def foldK[G[_], A](fga: F[G[A]])(implicit G: MonoidK[G]): G[A] =
    fold(fga)(G.algebra)

  /**
   * Compose this foldable instance with one for G creating Foldable[F[G]]
   */
  def compose[G[_]](implicit GG: Foldable[G]): Foldable[λ[α => F[G[α]]]] =
    new CompositeFoldable[F, G] {
      implicit def F: Foldable[F] = self
      implicit def G: Foldable[G] = GG
    }
}

/**
 * Methods that apply to 2 nested Foldable instances
 */
trait CompositeFoldable[F[_], G[_]] extends Foldable[λ[α => F[G[α]]]] {
  implicit def F: Foldable[F]
  implicit def G: Foldable[G]

  /**
   * Left assocative fold on F[G[A]] using 'f'
   */
  def foldLeft[A, B](fa: F[G[A]], b: B)(f: (B, A) => B): B =
    F.foldLeft(fa, b)((b, a) => G.foldLeft(a, b)(f))

  /**
   * Left assocative fold on F[G[A]] using 'f'
   */
  override def foldRight[A, B](fa: F[G[A]], b: B)(f: (A, B) => B): B =
    F.foldRight(fa, b)((a, b) => G.foldRight(a, b)(f))

  /**
   * Right associative lazy fold on `F` using the folding function 'f'.
   */
  def foldLazy[A, B](fa: F[G[A]], b: Lazy[B])(f: A => Fold[B]): Lazy[B] =
    F.foldLazy(fa, b) { ga =>
      Fold.Continue(b => G.foldLazy(ga, Lazy.eager(b))(a => f(a)).force)
    }
}
