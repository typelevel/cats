package cats

import scala.collection.mutable
import cats.instances.either._
import cats.kernel.CommutativeMonoid
import simulacrum.typeclass
import Foldable.sentinel

/**
 * Data structures that can be folded to a summary value.
 *
 * In the case of a collection (such as `List` or `Vector`), these
 * methods will fold together (combine) the values contained in the
 * collection to produce a single result. Most collection types have
 * `foldLeft` methods, which will usually be used by the associated
 * `Foldable[_]` instance.
 *
 * Instances of Foldable should be ordered collections to allow for consistent folding.
 * Use the `UnorderedFoldable` type class if you want to fold over unordered collections.
 *
 * Foldable[F] is implemented in terms of two basic methods:
 *
 *  - `foldLeft(fa, b)(f)` eagerly folds `fa` from left-to-right.
 *  - `foldRight(fa, b)(f)` lazily folds `fa` from right-to-left.
 *
 * Beyond these it provides many other useful methods related to
 * folding over F[A] values.
 *
 * See: [[http://www.cs.nott.ac.uk/~pszgmh/fold.pdf A tutorial on the universality and expressiveness of fold]]
 */
@typeclass trait Foldable[F[_]] extends UnorderedFoldable[F] { self =>

  /**
   * Left associative fold on 'F' using the function 'f'.
   *
   * Example:
   * {{{
   * scala> import cats.Foldable, cats.implicits._
   * scala> val fa = Option(1)
   *
   * Folding by addition to zero:
   * scala> Foldable[Option].foldLeft(fa, Option(0))((a, n) => a.map(_ + n))
   * res0: Option[Int] = Some(1)
   * }}}
   *
   * With syntax extensions, `foldLeft` can be used like:
   * {{{
   * Folding `Option` with addition from zero:
   * scala> fa.foldLeft(Option(0))((a, n) => a.map(_ + n))
   * res1: Option[Int] = Some(1)
   *
   * There's also an alias `foldl` which is equivalent:
   * scala> fa.foldl(Option(0))((a, n) => a.map(_ + n))
   * res2: Option[Int] = Some(1)
   * }}}
   */
  def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B

  /**
   * Right associative lazy fold on `F` using the folding function 'f'.
   *
   * This method evaluates `lb` lazily (in some cases it will not be
   * needed), and returns a lazy value. We are using `(A, Eval[B]) =>
   * Eval[B]` to support laziness in a stack-safe way. Chained
   * computation should be performed via .map and .flatMap.
   *
   * For more detailed information about how this method works see the
   * documentation for `Eval[_]`.
   *
   * Example:
   * {{{
   * scala> import cats.Foldable, cats.Eval, cats.implicits._
   * scala> val fa = Option(1)
   *
   * Folding by addition to zero:
   * scala> val folded1 = Foldable[Option].foldRight(fa, Eval.now(0))((n, a) => a.map(_ + n))
   * Since `foldRight` yields a lazy computation, we need to force it to inspect the result:
   * scala> folded1.value
   * res0: Int = 1
   *
   * With syntax extensions, we can write the same thing like this:
   * scala> val folded2 = fa.foldRight(Eval.now(0))((n, a) => a.map(_ + n))
   * scala> folded2.value
   * res1: Int = 1
   *
   * Unfortunately, since `foldRight` is defined on many collections - this
   * extension clashes with the operation defined in `Foldable`.
   *
   * To get past this and make sure you're getting the lazy `foldRight` defined
   * in `Foldable`, there's an alias `foldr`:
   * scala> val folded3 = fa.foldr(Eval.now(0))((n, a) => a.map(_ + n))
   * scala> folded3.value
   * res1: Int = 1
   * }}}
   */
  def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B]

  def reduceLeftToOption[A, B](fa: F[A])(f: A => B)(g: (B, A) => B): Option[B] =
    foldLeft(fa, Option.empty[B]) {
      case (Some(b), a) => Some(g(b, a))
      case (None, a)    => Some(f(a))
    }

  def reduceRightToOption[A, B](fa: F[A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[Option[B]] =
    foldRight(fa, Now(Option.empty[B])) { (a, lb) =>
      lb.flatMap {
        case Some(b) => g(a, Now(b)).map(Some(_))
        case None    => Later(Some(f(a)))
      }
    }

  /**
   * Reduce the elements of this structure down to a single value by applying
   * the provided aggregation function in a left-associative manner.
   *
   * @return `None` if the structure is empty, otherwise the result of combining
   * the cumulative left-associative result of the `f` operation over all of the
   * elements.
   *
   * @see [[reduceRightOption]] for a right-associative alternative.
   *
   * @see [[Reducible#reduceLeft]] for a version that doesn't need to return an
   * `Option` for structures that are guaranteed to be non-empty.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> val l = List(6, 3, 2)
   * This is equivalent to (6 - 3) - 2
   * scala> Foldable[List].reduceLeftOption(l)(_ - _)
   * res0: Option[Int] = Some(1)
   *
   * scala> Foldable[List].reduceLeftOption(List.empty[Int])(_ - _)
   * res1: Option[Int] = None
   * }}}
   */
  def reduceLeftOption[A](fa: F[A])(f: (A, A) => A): Option[A] =
    reduceLeftToOption(fa)(identity)(f)

  /**
   * Reduce the elements of this structure down to a single value by applying
   * the provided aggregation function in a right-associative manner.
   *
   * @return `None` if the structure is empty, otherwise the result of combining
   * the cumulative right-associative result of the `f` operation over the
   * `A` elements.
   *
   * @see [[reduceLeftOption]] for a left-associative alternative
   *
   * @see [[Reducible#reduceRight]] for a version that doesn't need to return an
   * `Option` for structures that are guaranteed to be non-empty.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> val l = List(6, 3, 2)
   * This is eqivalent to 6 - (3 - 2)
   * scala> Foldable[List].reduceRightOption(l)((current, rest) => rest.map(current - _)).value
   * res0: Option[Int] = Some(5)
   *
   * scala> Foldable[List].reduceRightOption(List.empty[Int])((current, rest) => rest.map(current - _)).value
   * res1: Option[Int] = None
   * }}}
   */
  def reduceRightOption[A](fa: F[A])(f: (A, Eval[A]) => Eval[A]): Eval[Option[A]] =
    reduceRightToOption(fa)(identity)(f)

  /**
   * Find the minimum `A` item in this structure according to the `Order[A]`.
   *
   * @return `None` if the structure is empty, otherwise the minimum element
   * wrapped in a `Some`.
   *
   * @see [[Reducible#minimum]] for a version that doesn't need to return an
   * `Option` for structures that are guaranteed to be non-empty.
   *
   * @see [[maximumOption]] for maximum instead of minimum.
   */
  def minimumOption[A](fa: F[A])(implicit A: Order[A]): Option[A] =
    reduceLeftOption(fa)(A.min)

  /**
   * Find the maximum `A` item in this structure according to the `Order[A]`.
   *
   * @return `None` if the structure is empty, otherwise the maximum element
   * wrapped in a `Some`.
   *
   * @see [[Reducible#maximum]] for a version that doesn't need to return an
   * `Option` for structures that are guaranteed to be non-empty.
   *
   * @see [[minimumOption]] for minimum instead of maximum.
   */
  def maximumOption[A](fa: F[A])(implicit A: Order[A]): Option[A] =
    reduceLeftOption(fa)(A.max)

  /**
   * Find the minimum `A` item in this structure according to an `Order.by(f)`.
   *
   * @return `None` if the structure is empty, otherwise the minimum element
   * wrapped in a `Some`.
   *
   * @see [[Reducible#minimum]] for a version that doesn't need to return an
   * `Option` for structures that are guaranteed to be non-empty.
   *
   * @see [[maximumOptionBy]] for maximum instead of minimum.
   */
  def minimumOptionBy[A, B: Order](fa: F[A])(f: A => B)(implicit F: Foldable[F]): Option[A] =
    F.minimumOption(fa)(Order.by(f))

  /**
   * Find the maximum `A` item in this structure according to an `Order.by(f)`.
   *
   * @return `None` if the structure is empty, otherwise the maximum element
   * wrapped in a `Some`.
   *
   * @see [[Reducible#maximum]] for a version that doesn't need to return an
   * `Option` for structures that are guaranteed to be non-empty.
   *
   * @see [[minimumOptionBy]] for minimum instead of maximum.
   */
  def maximumOptionBy[A, B: Order](fa: F[A])(f: A => B)(implicit F: Foldable[F]): Option[A] =
    F.maximumOption(fa)(Order.by(f))

  /**
   * Get the element at the index of the `Foldable`.
   */
  def get[A](fa: F[A])(idx: Long): Option[A] =
    if (idx < 0L) None
    else
      foldM[Either[A, *], A, Long](fa, 0L) { (i, a) =>
        if (i == idx) Left(a) else Right(i + 1L)
      } match {
        case Left(a)  => Some(a)
        case Right(_) => None
      }

  def collectFirst[A, B](fa: F[A])(pf: PartialFunction[A, B]): Option[B] =
    foldRight(fa, Eval.now(Option.empty[B])) { (a, lb) =>
      // trick from TraversableOnce, used to avoid calling both isDefined and apply (or calling lift)
      val x = pf.applyOrElse(a, sentinel)
      if (x.asInstanceOf[AnyRef] ne sentinel) Eval.now(Some(x.asInstanceOf[B]))
      else lb
    }.value

  /**
   * Like `collectFirst` from `scala.collection.Traversable` but takes `A => Option[B]`
   * instead of `PartialFunction`s.
   * {{{
   * scala> import cats.implicits._
   * scala> val keys = List(1, 2, 4, 5)
   * scala> val map = Map(4 -> "Four", 5 -> "Five")
   * scala> keys.collectFirstSome(map.get)
   * res0: Option[String] = Some(Four)
   * scala> val map2 = Map(6 -> "Six", 7 -> "Seven")
   * scala> keys.collectFirstSome(map2.get)
   * res1: Option[String] = None
   * }}}
   */
  def collectFirstSome[A, B](fa: F[A])(f: A => Option[B]): Option[B] =
    foldRight(fa, Eval.now(Option.empty[B])) { (a, lb) =>
      val ob = f(a)
      if (ob.isDefined) Eval.now(ob) else lb
    }.value

  /**
   * Fold implemented using the given Monoid[A] instance.
   */
  def fold[A](fa: F[A])(implicit A: Monoid[A]): A =
    foldLeft(fa, A.empty) { (acc, a) =>
      A.combine(acc, a)
    }

  /**
   * Alias for [[fold]].
   */
  def combineAll[A: Monoid](fa: F[A]): A = fold(fa)

  /**
   * Fold implemented by mapping `A` values into `B` and then
   * combining them using the given `Monoid[B]` instance.
   */
  def foldMap[A, B](fa: F[A])(f: A => B)(implicit B: Monoid[B]): B =
    foldLeft(fa, B.empty)((b, a) => B.combine(b, f(a)))

  /**
   * Perform a stack-safe monadic left fold from the source context `F`
   * into the target monad `G`.
   *
   * This method can express short-circuiting semantics. Even when
   * `fa` is an infinite structure, this method can potentially
   * terminate if the `foldRight` implementation for `F` and the
   * `tailRecM` implementation for `G` are sufficiently lazy.
   *
   * Instances for concrete structures (e.g. `List`) will often
   * have a more efficient implementation than the default one
   * in terms of `foldRight`.
   */
  def foldM[G[_], A, B](fa: F[A], z: B)(f: (B, A) => G[B])(implicit G: Monad[G]): G[B] = {
    val src = Foldable.Source.fromFoldable(fa)(self)
    G.tailRecM((z, src)) {
      case (b, src) =>
        src.uncons match {
          case Some((a, src)) => G.map(f(b, a))(b => Left((b, src.value)))
          case None           => G.pure(Right(b))
        }
    }
  }

  /**
   * Alias for [[foldM]].
   */
  final def foldLeftM[G[_], A, B](fa: F[A], z: B)(f: (B, A) => G[B])(implicit G: Monad[G]): G[B] =
    foldM(fa, z)(f)

  /**
   * Monadic folding on `F` by mapping `A` values to `G[B]`, combining the `B`
   * values using the given `Monoid[B]` instance.
   *
   * Similar to [[foldM]], but using a `Monoid[B]`.
   *
   * {{{
   * scala> import cats.Foldable
   * scala> import cats.implicits._
   * scala> val evenNumbers = List(2,4,6,8,10)
   * scala> val evenOpt: Int => Option[Int] =
   *      |   i => if (i % 2 == 0) Some(i) else None
   * scala> Foldable[List].foldMapM(evenNumbers)(evenOpt)
   * res0: Option[Int] = Some(30)
   * scala> Foldable[List].foldMapM(evenNumbers :+ 11)(evenOpt)
   * res1: Option[Int] = None
   * }}}
   */
  def foldMapM[G[_], A, B](fa: F[A])(f: A => G[B])(implicit G: Monad[G], B: Monoid[B]): G[B] =
    foldM(fa, B.empty)((b, a) => G.map(f(a))(B.combine(b, _)))

  /**
   * Traverse `F[A]` using `Applicative[G]`.
   *
   * `A` values will be mapped into `G[B]` and combined using
   * `Applicative#map2`.
   *
   * For example:
   *
   * {{{
   * scala> import cats.implicits._
   * scala> def parseInt(s: String): Option[Int] = Either.catchOnly[NumberFormatException](s.toInt).toOption
   * scala> val F = Foldable[List]
   * scala> F.traverse_(List("333", "444"))(parseInt)
   * res0: Option[Unit] = Some(())
   * scala> F.traverse_(List("333", "zzz"))(parseInt)
   * res1: Option[Unit] = None
   * }}}
   *
   * This method is primarily useful when `G[_]` represents an action
   * or effect, and the specific `A` aspect of `G[A]` is not otherwise
   * needed.
   */
  def traverse_[G[_], A, B](fa: F[A])(f: A => G[B])(implicit G: Applicative[G]): G[Unit] =
    foldRight(fa, Always(G.pure(()))) { (a, acc) =>
      G.map2Eval(f(a), acc) { (_, _) =>
        ()
      }
    }.value

  /**
   * Sequence `F[G[A]]` using `Applicative[G]`.
   *
   * This is similar to `traverse_` except it operates on `F[G[A]]`
   * values, so no additional functions are needed.
   *
   * For example:
   *
   * {{{
   * scala> import cats.implicits._
   * scala> val F = Foldable[List]
   * scala> F.sequence_(List(Option(1), Option(2), Option(3)))
   * res0: Option[Unit] = Some(())
   * scala> F.sequence_(List(Option(1), None, Option(3)))
   * res1: Option[Unit] = None
   * }}}
   */
  def sequence_[G[_]: Applicative, A](fga: F[G[A]]): G[Unit] =
    traverse_(fga)(identity)

  /**
   * Fold implemented using the given `MonoidK[G]` instance.
   *
   * This method is identical to fold, except that we use the universal monoid (`MonoidK[G]`)
   * to get a `Monoid[G[A]]` instance.
   *
   * For example:
   *
   * {{{
   * scala> import cats.implicits._
   * scala> val F = Foldable[List]
   * scala> F.foldK(List(1 :: 2 :: Nil, 3 :: 4 :: 5 :: Nil))
   * res0: List[Int] = List(1, 2, 3, 4, 5)
   * }}}
   */
  def foldK[G[_], A](fga: F[G[A]])(implicit G: MonoidK[G]): G[A] =
    fold(fga)(G.algebra)

  /**
   * Find the first element matching the predicate, if one exists.
   */
  def find[A](fa: F[A])(f: A => Boolean): Option[A] =
    foldRight(fa, Now(Option.empty[A])) { (a, lb) =>
      if (f(a)) Now(Some(a)) else lb
    }.value

  /**
   * Check whether at least one element satisfies the predicate.
   *
   * If there are no elements, the result is `false`.
   */
  override def exists[A](fa: F[A])(p: A => Boolean): Boolean =
    foldRight(fa, Eval.False) { (a, lb) =>
      if (p(a)) Eval.True else lb
    }.value

  /**
   * Check whether all elements satisfy the predicate.
   *
   * If there are no elements, the result is `true`.
   */
  override def forall[A](fa: F[A])(p: A => Boolean): Boolean =
    foldRight(fa, Eval.True) { (a, lb) =>
      if (p(a)) lb else Eval.False
    }.value

  /**
   * Check whether at least one element satisfies the effectful predicate.
   *
   * If there are no elements, the result is `false`.  `existsM` short-circuits,
   * i.e. once a `true` result is encountered, no further effects are produced.
   *
   * For example:
   *
   * {{{
   * scala> import cats.implicits._
   * scala> val F = Foldable[List]
   * scala> F.existsM(List(1,2,3,4))(n => Option(n <= 4))
   * res0: Option[Boolean] = Some(true)
   *
   * scala> F.existsM(List(1,2,3,4))(n => Option(n > 4))
   * res1: Option[Boolean] = Some(false)
   *
   * scala> F.existsM(List(1,2,3,4))(n => if (n <= 2) Option(true) else Option(false))
   * res2: Option[Boolean] = Some(true)
   *
   * scala> F.existsM(List(1,2,3,4))(n => if (n <= 2) Option(true) else None)
   * res3: Option[Boolean] = Some(true)
   *
   * scala> F.existsM(List(1,2,3,4))(n => if (n <= 2) None else Option(true))
   * res4: Option[Boolean] = None
   * }}}
   */
  def existsM[G[_], A](fa: F[A])(p: A => G[Boolean])(implicit G: Monad[G]): G[Boolean] =
    G.tailRecM(Foldable.Source.fromFoldable(fa)(self)) { src =>
      src.uncons match {
        case Some((a, src)) => G.map(p(a))(bb => if (bb) Right(true) else Left(src.value))
        case None           => G.pure(Right(false))
      }
    }

  /**
   * Check whether all elements satisfy the effectful predicate.
   *
   * If there are no elements, the result is `true`.  `forallM` short-circuits,
   * i.e. once a `false` result is encountered, no further effects are produced.
   *
   * For example:
   *
   * {{{
   * scala> import cats.implicits._
   * scala> val F = Foldable[List]
   * scala> F.forallM(List(1,2,3,4))(n => Option(n <= 4))
   * res0: Option[Boolean] = Some(true)
   *
   * scala> F.forallM(List(1,2,3,4))(n => Option(n <= 1))
   * res1: Option[Boolean] = Some(false)
   *
   * scala> F.forallM(List(1,2,3,4))(n => if (n <= 2) Option(true) else Option(false))
   * res2: Option[Boolean] = Some(false)
   *
   * scala> F.forallM(List(1,2,3,4))(n => if (n <= 2) Option(false) else None)
   * res3: Option[Boolean] = Some(false)
   *
   * scala> F.forallM(List(1,2,3,4))(n => if (n <= 2) None else Option(false))
   * res4: Option[Boolean] = None
   * }}}
   */
  def forallM[G[_], A](fa: F[A])(p: A => G[Boolean])(implicit G: Monad[G]): G[Boolean] =
    G.tailRecM(Foldable.Source.fromFoldable(fa)(self)) { src =>
      src.uncons match {
        case Some((a, src)) => G.map(p(a))(bb => if (!bb) Right(false) else Left(src.value))
        case None           => G.pure(Right(true))
      }
    }

  /**
   * Convert F[A] to a List[A].
   */
  def toList[A](fa: F[A]): List[A] =
    foldLeft(fa, mutable.ListBuffer.empty[A]) { (buf, a) =>
      buf += a
    }.toList

  /**
   * Separate this Foldable into a Tuple by a separating function `A => Either[B, C]`
   * Equivalent to `Functor#map` and then `Alternative#separate`.
   *
   * {{{
   * scala> import cats.implicits._
   * scala> val list = List(1,2,3,4)
   * scala> Foldable[List].partitionEither(list)(a => if (a % 2 == 0) Left(a.toString) else Right(a))
   * res0: (List[String], List[Int]) = (List(2, 4),List(1, 3))
   * scala> Foldable[List].partitionEither(list)(a => Right(a * 4))
   * res1: (List[Nothing], List[Int]) = (List(),List(4, 8, 12, 16))
   * }}}
   */
  def partitionEither[A, B, C](fa: F[A])(f: A => Either[B, C])(implicit A: Alternative[F]): (F[B], F[C]) = {
    import cats.instances.tuple._

    implicit val mb: Monoid[F[B]] = A.algebra[B]
    implicit val mc: Monoid[F[C]] = A.algebra[C]

    foldMap(fa)(
      a =>
        f(a) match {
          case Right(c) => (A.empty[B], A.pure(c))
          case Left(b)  => (A.pure(b), A.empty[C])
        }
    )
  }

  /**
   * Convert F[A] to a List[A], only including elements which match `p`.
   */
  def filter_[A](fa: F[A])(p: A => Boolean): List[A] =
    foldLeft(fa, mutable.ListBuffer.empty[A]) { (buf, a) =>
      if (p(a)) buf += a else buf
    }.toList

  /**
   * Convert F[A] to a List[A], retaining only initial elements which
   * match `p`.
   */
  def takeWhile_[A](fa: F[A])(p: A => Boolean): List[A] =
    foldRight(fa, Now(List.empty[A])) { (a, llst) =>
      if (p(a)) llst.map(a :: _) else Now(Nil)
    }.value

  /**
   * Convert F[A] to a List[A], dropping all initial elements which
   * match `p`.
   */
  def dropWhile_[A](fa: F[A])(p: A => Boolean): List[A] =
    foldLeft(fa, mutable.ListBuffer.empty[A]) { (buf, a) =>
      if (buf.nonEmpty || !p(a)) buf += a else buf
    }.toList

  /**
   * Returns true if there are no elements. Otherwise false.
   */
  override def isEmpty[A](fa: F[A]): Boolean =
    foldRight(fa, Eval.True)((_, _) => Eval.False).value

  override def nonEmpty[A](fa: F[A]): Boolean =
    !isEmpty(fa)

  /**
   * Intercalate/insert an element between the existing elements while folding.
   *
   * {{{
   * scala> import cats.implicits._
   * scala> Foldable[List].intercalate(List("a","b","c"), "-")
   * res0: String = a-b-c
   * scala> Foldable[List].intercalate(List("a"), "-")
   * res1: String = a
   * scala> Foldable[List].intercalate(List.empty[String], "-")
   * res2: String = ""
   * scala> Foldable[Vector].intercalate(Vector(1,2,3), 1)
   * res3: Int = 8
   * }}}
   */
  def intercalate[A](fa: F[A], a: A)(implicit A: Monoid[A]): A =
    A.combineAll(intersperseList(toList(fa), a))

  protected def intersperseList[A](xs: List[A], x: A): List[A] = {
    val bld = List.newBuilder[A]
    val it = xs.iterator
    if (it.hasNext) {
      bld += it.next
      while (it.hasNext) {
        bld += x
        bld += it.next
      }
    }
    bld.result
  }

  def compose[G[_]: Foldable]: Foldable[λ[α => F[G[α]]]] =
    new ComposedFoldable[F, G] {
      val F = self
      val G = Foldable[G]
    }

  override def unorderedFold[A: CommutativeMonoid](fa: F[A]): A = fold(fa)

  override def unorderedFoldMap[A, B: CommutativeMonoid](fa: F[A])(f: (A) => B): B =
    foldMap(fa)(f)
}

object Foldable {
  private val sentinel: Function1[Any, Any] = new scala.runtime.AbstractFunction1[Any, Any] { def apply(a: Any) = this }

  def iterateRight[A, B](iterable: Iterable[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
    def loop(it: Iterator[A]): Eval[B] =
      Eval.defer(if (it.hasNext) f(it.next, loop(it)) else lb)

    Eval.always(iterable.iterator).flatMap(loop)
  }

  /**
   * Isomorphic to
   *
   *     type Source[+A] = () => Option[(A, Source[A])]
   *
   * (except that recursive type aliases are not allowed).
   *
   * It could be made a value class after
   * https://github.com/scala/bug/issues/9600 is resolved.
   */
  sealed abstract private[cats] class Source[+A] {
    def uncons: Option[(A, Eval[Source[A]])]
  }

  private[cats] object Source {
    val Empty: Source[Nothing] = new Source[Nothing] {
      def uncons = None
    }

    def cons[A](a: A, src: Eval[Source[A]]): Source[A] = new Source[A] {
      def uncons = Some((a, src))
    }

    def fromFoldable[F[_], A](fa: F[A])(implicit F: Foldable[F]): Source[A] =
      F.foldRight[A, Source[A]](fa, Now(Empty))((a, evalSrc) => Later(cons(a, evalSrc))).value
  }
}
