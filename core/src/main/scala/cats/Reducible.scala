package cats

import cats.data.NonEmptyList

import simulacrum.typeclass

/**
 * Data structures that can be reduced to a summary value.
 *
 * `Reducible` is like a non-empty `Foldable`. In addition to the fold
 * methods it provides reduce methods which do not require an initial
 * value.
 *
 * In addition to the methods needed by `Foldable`, `Reducible` is
 * implemented in terms of two methods:
 *
 *  - `reduceLeftTo(fa)(f)(g)` eagerly reduces with an additional mapping function
 *  - `reduceRightTo(fa)(f)(g)` lazily reduces with an additional mapping function
 */
@typeclass trait Reducible[F[_]] extends Foldable[F] { self =>

  /**
   * Left-associative reduction on `F` using the function `f`.
   *
   * Implementations should override this method when possible.
   */
  def reduceLeft[A](fa: F[A])(f: (A, A) => A): A =
    reduceLeftTo(fa)(identity)(f)

  /**
   * Right-associative reduction on `F` using the function `f`.
   */
  def reduceRight[A](fa: F[A])(f: (A, Eval[A]) => Eval[A]): Eval[A] =
    reduceRightTo(fa)(identity)(f)

  /**
   * Reduce a `F[A]` value using the given `Semigroup[A]`.
   */
  def reduce[A](fa: F[A])(implicit A: Semigroup[A]): A =
    reduceLeft(fa)(A.combine)

  /**
   * Reduce a `F[G[A]]` value using `SemigroupK[G]`, a universal
   * semigroup for `G[_]`.
   *
   * This method is a generalization of `reduce`.
   */
  def reduceK[G[_], A](fga: F[G[A]])(implicit G: SemigroupK[G]): G[A] =
    reduce(fga)(G.algebra)

  /**
   * Apply `f` to each element of `fa` and combine them using the
   * given `Semigroup[B]`.
   */
  def reduceMap[A, B](fa: F[A])(f: A => B)(implicit B: Semigroup[B]): B =
    reduceLeftTo(fa)(f)((b, a) => B.combine(b, f(a)))

  /**
   * Apply `f` to the "initial element" of `fa` and combine it with
   * every other value using the given function `g`.
   */
  def reduceLeftTo[A, B](fa: F[A])(f: A => B)(g: (B, A) => B): B

  /**
   *  Monadic variant of [[reduceLeftTo]]
   */
  def reduceLeftM[G[_], A, B](fa: F[A])(f: A => G[B])(g: (B, A) => G[B])(implicit G: FlatMap[G]): G[B] =
    reduceLeftTo(fa)(f)((gb, a) => G.flatMap(gb)(g(_, a)))

  /**
   * Monadic reducing by mapping the `A` values to `G[B]`. combining
   * the `B` values using the given `Semigroup[B]` instance.
   *
   * Similar to [[reduceLeftM]], but using a `Semigroup[B]`.
   *
   * {{{
   * scala> import cats.Reducible
   * scala> import cats.data.NonEmptyList
   * scala> import cats.implicits._
   * scala> val evenOpt: Int => Option[Int] =
   *      |   i => if (i % 2 == 0) Some(i) else None
   * scala> val allEven = NonEmptyList.of(2,4,6,8,10)
   * allEven: cats.data.NonEmptyList[Int] = NonEmptyList(2, 4, 6, 8, 10)
   * scala> val notAllEven = allEven ++ List(11)
   * notAllEven: cats.data.NonEmptyList[Int] = NonEmptyList(2, 4, 6, 8, 10, 11)
   * scala> Reducible[NonEmptyList].reduceMapM(allEven)(evenOpt)
   * res0: Option[Int] = Some(30)
   * scala> Reducible[NonEmptyList].reduceMapM(notAllEven)(evenOpt)
   * res1: Option[Int] = None
   * }}}
   */
  def reduceMapM[G[_], A, B](fa: F[A])(f: A => G[B])(implicit G: FlatMap[G], B: Semigroup[B]): G[B] =
    reduceLeftM(fa)(f)((b, a) => G.map(f(a))(B.combine(b, _)))

  /**
   * Overriden from [[Foldable]] for efficiency.
   */
  override def reduceLeftToOption[A, B](fa: F[A])(f: A => B)(g: (B, A) => B): Option[B] =
    Some(reduceLeftTo(fa)(f)(g))

  /**
   * Apply `f` to the "initial element" of `fa` and lazily combine it
   * with every other value using the given function `g`.
   */
  def reduceRightTo[A, B](fa: F[A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B]

  /**
   * Overriden from [[Foldable]] for efficiency.
   */
  override def reduceRightToOption[A, B](fa: F[A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[Option[B]] =
    reduceRightTo(fa)(f)(g).map(Some(_))

  /**
   * Traverse `F[A]` using `Apply[G]`.
   *
   * `A` values will be mapped into `G[B]` and combined using
   * `Apply#map2`.
   *
   * This method is similar to [[Foldable.traverse_]]. There are two
   * main differences:
   *
   * 1. We only need an [[Apply]] instance for `G` here, since we
   * don't need to call [[Applicative.pure]] for a starting value.
   * 2. This performs a strict left-associative traversal and thus
   * must always traverse the entire data structure. Prefer
   * [[Foldable.traverse_]] if you have an [[Applicative]] instance
   * available for `G` and want to take advantage of short-circuiting
   * the traversal.
   */
  def traverse1_[G[_], A, B](fa: F[A])(f: A => G[B])(implicit G: Apply[G]): G[Unit] =
    G.map(reduceLeftTo(fa)(f)((x, y) => G.map2(x, f(y))((_, b) => b)))(_ => ())

  /**
   * Sequence `F[G[A]]` using `Apply[G]`.
   *
   * This method is similar to [[Foldable.sequence_]] but requires only
   * an [[Apply]] instance for `G` instead of [[Applicative]]. See the
   * [[traverse1_]] documentation for a description of the differences.
   */
  def sequence1_[G[_], A](fga: F[G[A]])(implicit G: Apply[G]): G[Unit] =
    G.map(reduceLeft(fga)((x, y) => G.map2(x, y)((_, b) => b)))(_ => ())

  def toNonEmptyList[A](fa: F[A]): NonEmptyList[A] =
    reduceRightTo(fa)(a => NonEmptyList(a, Nil)) { (a, lnel) =>
      lnel.map { case NonEmptyList(h, t) => NonEmptyList(a, h :: t) }
    }.value

  def compose[G[_]: Reducible]: Reducible[λ[α => F[G[α]]]] =
    new ComposedReducible[F, G] {
      val F = self
      val G = Reducible[G]
    }

  def minimum[A](fa: F[A])(implicit A: Order[A]): A =
    reduceLeft(fa)(A.min)

  def maximum[A](fa: F[A])(implicit A: Order[A]): A =
    reduceLeft(fa)(A.max)

  /**
   * Intercalate/insert an element between the existing elements while reducing.
   *
   * {{{
   * scala> import cats.implicits._
   * scala> import cats.data.NonEmptyList
   * scala> val nel = NonEmptyList.of("a", "b", "c")
   * scala> Reducible[NonEmptyList].intercalate1(nel, "-")
   * res0: String = a-b-c
   * scala> Reducible[NonEmptyList].intercalate1(NonEmptyList.of("a"), "-")
   * res1: String = a
   * }}}
   */
  def intercalate1[A](fa: F[A], a: A)(implicit A: Semigroup[A]): A =
    toNonEmptyList(fa) match {
      case NonEmptyList(hd, Nil) => hd
      case NonEmptyList(hd, tl) =>
        Reducible[NonEmptyList].reduce(NonEmptyList(hd, a :: intersperseList(tl, a)))
    }

  override def isEmpty[A](fa: F[A]): Boolean = false

  override def nonEmpty[A](fa: F[A]): Boolean = true

  override def minimumOption[A](fa: F[A])(implicit A: Order[A]): Option[A] =
    Some(minimum(fa))

  override def maximumOption[A](fa: F[A])(implicit A: Order[A]): Option[A] =
    Some(maximum(fa))
}

/**
 * This class defines a `Reducible[F]` in terms of a `Foldable[G]`
 * together with a `split` method, `F[A]` => `(A, G[A])`.
 *
 * This class can be used on any type where the first value (`A`) and
 * the "rest" of the values (`G[A]`) can be easily found.
 */
abstract class NonEmptyReducible[F[_], G[_]](implicit G: Foldable[G]) extends Reducible[F] {
  def split[A](fa: F[A]): (A, G[A])

  def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B = {
    val (a, ga) = split(fa)
    G.foldLeft(ga, f(b, a))(f)
  }

  def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    Always(split(fa)).flatMap { case (a, ga) =>
      f(a, G.foldRight(ga, lb)(f))
    }

  def reduceLeftTo[A, B](fa: F[A])(f: A => B)(g: (B, A) => B): B = {
    val (a, ga) = split(fa)
    G.foldLeft(ga, f(a))((b, a) => g(b, a))
  }

  def reduceRightTo[A, B](fa: F[A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B] =
    Always(split(fa)).flatMap { case (a, ga) =>
      G.reduceRightToOption(ga)(f)(g).flatMap {
        case Some(b) => g(a, Now(b))
        case None => Later(f(a))
      }
    }

  override def size[A](fa: F[A]): Long = {
    val (_, tail) = split(fa)
    1 + G.size(tail)
  }

  override def get[A](fa: F[A])(idx: Long): Option[A] =
    if (idx == 0L) Some(split(fa)._1) else G.get(split(fa)._2)(idx)

  override def fold[A](fa: F[A])(implicit A: Monoid[A]): A = {
    val (a, ga) = split(fa)
    A.combine(a, G.fold(ga))
  }

  override def foldM[H[_], A, B](fa: F[A], z: B)(f: (B, A) => H[B])(implicit H: Monad[H]): H[B] = {
    val (a, ga) = split(fa)
    H.flatMap(f(z, a))(G.foldM(ga, _)(f))
  }

  override def find[A](fa: F[A])(f: A => Boolean): Option[A] = {
    val (a, ga) = split(fa)
    if (f(a)) Some(a) else G.find(ga)(f)
  }

  override def exists[A](fa: F[A])(p: A => Boolean): Boolean = {
    val (a, ga) = split(fa)
    p(a) || G.exists(ga)(p)
  }

  override def forall[A](fa: F[A])(p: A => Boolean): Boolean = {
    val (a, ga) = split(fa)
    p(a) && G.forall(ga)(p)
  }

  override def toList[A](fa: F[A]): List[A] = {
    val (a, ga) = split(fa)
    a :: G.toList(ga)
  }

  override def toNonEmptyList[A](fa: F[A]): NonEmptyList[A] = {
    val (a, ga) = split(fa)
    NonEmptyList(a, G.toList(ga))
  }

  override def filter_[A](fa: F[A])(p: A => Boolean): List[A] = {
    val (a, ga) = split(fa)
    val filteredTail = G.filter_(ga)(p)
    if (p(a)) a :: filteredTail else filteredTail
  }

  override def takeWhile_[A](fa: F[A])(p: A => Boolean): List[A] = {
    val (a, ga) = split(fa)
    if (p(a)) a :: G.takeWhile_(ga)(p) else Nil
  }

  override def dropWhile_[A](fa: F[A])(p: A => Boolean): List[A] = {
    val (a, ga) = split(fa)
    if (p(a)) G.dropWhile_(ga)(p) else a :: G.toList(ga)
  }
}
