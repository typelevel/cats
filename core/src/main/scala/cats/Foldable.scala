/*
 * Copyright (c) 2015 Typelevel
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package cats

import scala.collection.mutable
import cats.kernel.CommutativeMonoid

import Foldable.{sentinel, Source}

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
trait Foldable[F[_]] extends UnorderedFoldable[F] with FoldableNFunctions[F] { self =>

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

  def foldRightDefer[G[_]: Defer, A, B](fa: F[A], gb: G[B])(fn: (A, G[B]) => G[B]): G[B] = {
    def loop(source: Source[A]): G[B] = {
      source.uncons match {
        case Some((next, s)) => fn(next, Defer[G].defer(loop(s.value)))
        case None            => gb
      }
    }
    Defer[G].defer(loop(Source.fromFoldable(fa)(self)))
  }

  def reduceLeftToOption[A, B](fa: F[A])(f: A => B)(g: (B, A) => B): Option[B] =
    foldLeft(fa, Option.empty[B]) {
      case (Some(b), a) => Some(g(b, a))
      case (None, a)    => Some(f(a))
    }

  def reduceRightToOption[A, B](fa: F[A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[Option[B]] = {
    Source.fromFoldable(fa)(self).uncons match {
      case Some((first, s)) =>
        def loop(now: A, source: Source[A]): Eval[B] =
          source.uncons match {
            case Some((next, s)) => g(now, Eval.defer(loop(next, s.value)))
            case None            => Eval.later(f(now))
          }

        Eval.defer(loop(first, s.value).map(Some(_)))
      case None => Eval.now(None)
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
   * scala> import cats.syntax.all._
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
   * scala> import cats.syntax.all._
   * scala> val l = List(6, 3, 2)
   * This is equivalent to 6 - (3 - 2)
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
   * @see [[Reducible#minimumBy]] for a version that doesn't need to return an
   * `Option` for structures that are guaranteed to be non-empty.
   *
   * @see [[maximumByOption]] for maximum instead of minimum.
   */
  def minimumByOption[A, B: Order](fa: F[A])(f: A => B): Option[A] =
    minimumOption(fa)(Order.by(f))

  /**
   * Find the maximum `A` item in this structure according to an `Order.by(f)`.
   *
   * @return `None` if the structure is empty, otherwise the maximum element
   * wrapped in a `Some`.
   *
   * @see [[Reducible#maximumBy]] for a version that doesn't need to return an
   * `Option` for structures that are guaranteed to be non-empty.
   *
   * @see [[minimumByOption]] for minimum instead of maximum.
   */
  def maximumByOption[A, B: Order](fa: F[A])(f: A => B): Option[A] =
    maximumOption(fa)(Order.by(f))

  /**
   * Find all the minimum `A` items in this structure.
   * For all elements in the result Order.eqv(x, y) is true. Preserves order.
   *
   * @see [[Reducible#minimumNel]] for a version that doesn't need to return an
   * `Option` for structures that are guaranteed to be non-empty.
   *
   * @see [[maximumList]] for maximum instead of minimum.
   */
  def minimumList[A](fa: F[A])(implicit A: Order[A]): List[A] =
    foldLeft(fa, List.empty[A]) {
      case (l @ (b :: _), a) if A.compare(a, b) > 0  => l
      case (l @ (b :: _), a) if A.compare(a, b) == 0 => a :: l
      case (_, a)                                    => a :: Nil
    }.reverse

  /**
   * Find all the maximum `A` items in this structure.
   * For all elements in the result Order.eqv(x, y) is true. Preserves order.
   *
   * @see [[Reducible#maximumNel]] for a version that doesn't need to return an
   * `Option` for structures that are guaranteed to be non-empty.
   *
   * @see [[minimumList]] for minimum instead of maximum.
   */
  def maximumList[A](fa: F[A])(implicit A: Order[A]): List[A] =
    foldLeft(fa, List.empty[A]) {
      case (l @ (b :: _), a) if A.compare(a, b) < 0  => l
      case (l @ (b :: _), a) if A.compare(a, b) == 0 => a :: l
      case (_, a)                                    => a :: Nil
    }.reverse

  /**
   * Find all the minimum `A` items in this structure according to an `Order.by(f)`.
   * For all elements in the result Order.eqv(x, y) is true. Preserves order.
   *
   * @see [[Reducible#minimumByNel]] for a version that doesn't need to return an
   * `Option` for structures that are guaranteed to be non-empty.
   *
   * @see [[maximumByList]] for maximum instead of minimum.
   */
  def minimumByList[A, B: Order](fa: F[A])(f: A => B): List[A] =
    minimumList(fa)(Order.by(f))

  /**
   * Find all the maximum `A` items in this structure according to an `Order.by(f)`.
   * For all elements in the result Order.eqv(x, y) is true. Preserves order.
   *
   * @see [[Reducible#maximumByNel]] for a version that doesn't need to return an
   * `Option` for structures that are guaranteed to be non-empty.
   *
   * @see [[minimumByList]] for minimum instead of maximum.
   */
  def maximumByList[A, B: Order](fa: F[A])(f: A => B): List[A] =
    maximumList(fa)(Order.by(f))

  def sumAll[A](fa: F[A])(implicit A: Numeric[A]): A =
    foldLeft(fa, A.zero)(A.plus)

  def productAll[A](fa: F[A])(implicit A: Numeric[A]): A =
    foldLeft(fa, A.one)(A.times)

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
   * scala> import cats.syntax.all._
   * scala> val keys = List(1, 2, 4, 5)
   * scala> val map = Map(4 -> "Four", 5 -> "Five")
   * scala> keys.collectFirstSome(map.get)
   * res0: Option[String] = Some(Four)
   * scala> val map2 = Map(6 -> "Six", 7 -> "Seven")
   * scala> keys.collectFirstSome(map2.get)
   * res1: Option[String] = None
   * }}}
   */
  def collectFirstSome[A, B](fa: F[A])(f: A => Option[B]): Option[B] = {
    val maybeEmpty = toIterable(fa).iterator.map(f).dropWhile(_.isEmpty)
    if (maybeEmpty.hasNext) maybeEmpty.next()
    else None
  }

  /**
   * Monadic version of `collectFirstSome`.
   *
   * If there are no elements, the result is `None`. `collectFirstSomeM` short-circuits,
   * i.e. once a Some element is found, no further effects are produced.
   *
   * For example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> def parseInt(s: String): Either[String, Int] = Either.catchOnly[NumberFormatException](s.toInt).leftMap(_.getMessage)
   * scala> val keys1 = List("1", "2", "4", "5")
   * scala> val map1 = Map(4 -> "Four", 5 -> "Five")
   * scala> Foldable[List].collectFirstSomeM(keys1)(parseInt(_) map map1.get)
   * res0: scala.util.Either[String,Option[String]] = Right(Some(Four))
   *
   * scala> val map2 = Map(6 -> "Six", 7 -> "Seven")
   * scala> Foldable[List].collectFirstSomeM(keys1)(parseInt(_) map map2.get)
   * res1: scala.util.Either[String,Option[String]] = Right(None)
   *
   * scala> val keys2 = List("1", "x", "4", "5")
   * scala> Foldable[List].collectFirstSomeM(keys2)(parseInt(_) map map1.get)
   * res2: scala.util.Either[String,Option[String]] = Left(For input string: "x")
   *
   * scala> val keys3 = List("1", "2", "4", "x")
   * scala> Foldable[List].collectFirstSomeM(keys3)(parseInt(_) map map1.get)
   * res3: scala.util.Either[String,Option[String]] = Right(Some(Four))
   * }}}
   */

  def collectFirstSomeM[G[_], A, B](fa: F[A])(f: A => G[Option[B]])(implicit G: Monad[G]): G[Option[B]] =
    G.tailRecM(Foldable.Source.fromFoldable(fa)(self))(_.uncons match {
      case Some((a, src)) =>
        G.map(f(a)) {
          case None => Left(src.value)
          case s    => Right(s)
        }
      case None => G.pure(Right(None))
    })

  /**
   * Tear down a subset of this structure using a `PartialFunction`.
   * {{{
   * scala> import cats.syntax.all._
   * scala> val xs = List(1, 2, 3, 4)
   * scala> Foldable[List].collectFold(xs) { case n if n % 2 == 0 => n }
   * res0: Int = 6
   * }}}
   */

  def collectFold[A, B](fa: F[A])(f: PartialFunction[A, B])(implicit B: Monoid[B]): B =
    foldLeft(fa, B.empty)((acc, a) => B.combine(acc, f.applyOrElse(a, (_: A) => B.empty)))

  /**
   * Tear down a subset of this structure using a `A => Option[M]`.
   * {{{
   * scala> import cats.syntax.all._
   * scala> val xs = List(1, 2, 3, 4)
   * scala> def f(n: Int): Option[Int] = if (n % 2 == 0) Some(n) else None
   * scala> Foldable[List].collectFoldSome(xs)(f)
   * res0: Int = 6
   * }}}
   */
  def collectFoldSome[A, B](fa: F[A])(f: A => Option[B])(implicit B: Monoid[B]): B =
    foldLeft(fa, B.empty)((acc, a) =>
      f(a) match {
        case Some(x) => B.combine(acc, x)
        case None    => acc
      }
    )

  /**
   * Fold implemented using the given `Monoid[A]` instance.
   */
  def fold[A](fa: F[A])(implicit A: Monoid[A]): A =
    A.combineAll(toIterable(fa))

  /**
   * Alias for [[fold]].
   */
  def combineAll[A: Monoid](fa: F[A]): A = fold(fa)

  def combineAllOption[A](fa: F[A])(implicit ev: Semigroup[A]): Option[A] =
    if (isEmpty(fa)) None else ev.combineAllOption(toIterable(fa))

  /**
   * Convert F[A] to an Iterable[A].
   *
   * This method may be overridden for the sake of performance, but implementers should take care
   * not to force a full materialization of the collection.
   */
  def toIterable[A](fa: F[A]): Iterable[A] =
    cats.compat.FoldableCompat.toIterable(fa)(self)

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
    G.tailRecM((z, src)) { case (b, src) =>
      src.uncons match {
        case Some((a, src)) => G.map(f(b, a))(b => Left((b, src.value)))
        case None           => G.pure(Right(b))
      }
    }
  }

  /**
   * Fold implemented using the given `Applicative[G]` and `Monoid[A]` instance.
   *
   * This method is similar to [[fold]], but may short-circuit.
   *
   * For example:
   *
   * {{{
   * scala> import cats.syntax.all._
   * scala> val F = Foldable[List]
   * scala> F.foldA(List(Either.right[String, Int](1), Either.right[String, Int](2)))
   * res0: Either[String, Int] = Right(3)
   * }}}
   */
  def foldA[G[_], A](fga: F[G[A]])(implicit G: Applicative[G], A: Monoid[A]): G[A] =
    foldMapA(fga)(identity)

  /**
   * Fold implemented by mapping `A` values into `B` in a context `G` and then
   * combining them using the `MonoidK[G]` instance.
   *
   * {{{
   * scala> import cats._, cats.implicits._
   * scala> val f: Int => Endo[String] = i => (s => s + i)
   * scala> val x: Endo[String] = Foldable[List].foldMapK(List(1, 2, 3))(f)
   * scala> val a = x("foo")
   * a: String = "foo321"
   * }}}
   */

  def foldMapK[G[_], A, B](fa: F[A])(f: A => G[B])(implicit G: MonoidK[G]): G[B] =
    foldRight(fa, Eval.now(G.empty[B])) { (a, evalGb) =>
      G.combineKEval(f(a), evalGb)
    }.value

  /**
   * Alias for [[foldM]].
   */
  final def foldLeftM[G[_], A, B](fa: F[A], z: B)(f: (B, A) => G[B])(implicit G: Monad[G]): G[B] =
    foldM(fa, z)(f)

  /**
   * Monadic folding on `F` by mapping `A` values to `G[B]`, combining the `B`
   * values using the given `Monoid[B]` instance.
   *
   * Similar to [[foldM]], but using a `Monoid[B]`. Will typically be more efficient than [[foldMapA]].
   *
   * {{{
   * scala> import cats.Foldable
   * scala> import cats.syntax.all._
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
   * Fold in an [[Applicative]] context by mapping the `A` values to `G[B]`. combining
   * the `B` values using the given `Monoid[B]` instance.
   *
   * Similar to [[foldMapM]], but will typically be less efficient.
   *
   * {{{
   * scala> import cats.Foldable
   * scala> import cats.syntax.all._
   * scala> val evenNumbers = List(2,4,6,8,10)
   * scala> val evenOpt: Int => Option[Int] =
   *      |   i => if (i % 2 == 0) Some(i) else None
   * scala> Foldable[List].foldMapA(evenNumbers)(evenOpt)
   * res0: Option[Int] = Some(30)
   * scala> Foldable[List].foldMapA(evenNumbers :+ 11)(evenOpt)
   * res1: Option[Int] = None
   * }}}
   */
  def foldMapA[G[_], A, B](fa: F[A])(f: A => G[B])(implicit G: Applicative[G], B: Monoid[B]): G[B] =
    foldRight(fa, Eval.now(G.pure(B.empty)))((a, egb) => G.map2Eval(f(a), egb)(B.combine)).value

  /**
   * Traverse `F[A]` using `Applicative[G]`.
   *
   * `A` values will be mapped into `G[B]` and combined using
   * `Applicative#map2`.
   *
   * For example:
   *
   * {{{
   * scala> import cats.syntax.all._
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
    foldRight(fa, Always(G.unit)) { (a, acc) =>
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
   * scala> import cats.syntax.all._
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
   * scala> import cats.syntax.all._
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
    toIterable(fa).find(f)

  /**
   * Find the first element matching the effectful predicate, if one exists.
   *
   * If there are no elements, the result is `None`. `findM` short-circuits,
   * i.e. once an element is found, no further effects are produced.
   *
   * For example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> val list = List(1,2,3,4)
   * scala> Foldable[List].findM(list)(n => (n >= 2).asRight[String])
   * res0: Either[String,Option[Int]] = Right(Some(2))
   *
   * scala> Foldable[List].findM(list)(n => (n > 4).asRight[String])
   * res1: Either[String,Option[Int]] = Right(None)
   *
   * scala> Foldable[List].findM(list)(n => Either.cond(n < 3, n >= 2, "error"))
   * res2: Either[String,Option[Int]] = Right(Some(2))
   *
   * scala> Foldable[List].findM(list)(n => Either.cond(n < 3, false, "error"))
   * res3: Either[String,Option[Int]] = Left(error)
   * }}}
   */

  def findM[G[_], A](fa: F[A])(p: A => G[Boolean])(implicit G: Monad[G]): G[Option[A]] =
    G.tailRecM(Foldable.Source.fromFoldable(fa)(self))(_.uncons match {
      case Some((a, src)) => G.map(p(a))(if (_) Right(Some(a)) else Left(src.value))
      case None           => G.pure(Right(None))
    })

  /**
   * Check whether at least one element satisfies the predicate.
   *
   * If there are no elements, the result is `false`.
   */
  override def exists[A](fa: F[A])(p: A => Boolean): Boolean =
    toIterable(fa).exists(p)

  /**
   * Check whether all elements satisfy the predicate.
   *
   * If there are no elements, the result is `true`.
   */
  override def forall[A](fa: F[A])(p: A => Boolean): Boolean =
    toIterable(fa).forall(p)

  /**
   * Check whether at least one element satisfies the effectful predicate.
   *
   * If there are no elements, the result is `false`.  `existsM` short-circuits,
   * i.e. once a `true` result is encountered, no further effects are produced.
   *
   * For example:
   *
   * {{{
   * scala> import cats.syntax.all._
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
   * scala> import cats.syntax.all._
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
   * scala> import cats.syntax.all._
   * scala> val list = List(1,2,3,4)
   * scala> Foldable[List].partitionEither(list)(a => if (a % 2 == 0) Left(a.toString) else Right(a))
   * res0: (List[String], List[Int]) = (List(2, 4),List(1, 3))
   * scala> Foldable[List].partitionEither(list)(a => Right(a * 4))
   * res1: (List[Nothing], List[Int]) = (List(),List(4, 8, 12, 16))
   * }}}
   */
  def partitionEither[A, B, C](fa: F[A])(f: A => Either[B, C])(implicit A: Alternative[F]): (F[B], F[C]) = {
    implicit val mb: Monoid[F[B]] = A.algebra[B]
    implicit val mc: Monoid[F[C]] = A.algebra[C]

    foldMap(fa)(a =>
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
    toIterable(fa).iterator.takeWhile(p).toList

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
   * scala> import cats.syntax.all._
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
    combineAllOption(fa)(A.intercalate(a)) match {
      case None    => A.empty
      case Some(a) => a
    }

  protected def intersperseList[A](xs: List[A], x: A): List[A] = {
    val bld = List.newBuilder[A]
    val it = xs.iterator
    if (it.hasNext) {
      bld += it.next()
      while (it.hasNext) {
        bld += x
        bld += it.next()
      }
    }
    bld.result()
  }

  def compose[G[_]: Foldable]: Foldable[λ[α => F[G[α]]]] =
    new ComposedFoldable[F, G] {
      val F = self
      val G = Foldable[G]
    }

  override def unorderedFold[A: CommutativeMonoid](fa: F[A]): A = fold(fa)

  override def unorderedFoldMap[A, B: CommutativeMonoid](fa: F[A])(f: (A) => B): B =
    foldMap(fa)(f)

  /**
   * Separate this Foldable into a Tuple by a separating function `A => H[B, C]` for some `Bifoldable[H]`
   * Equivalent to `Functor#map` and then `Alternative#separate`.
   *
   * {{{
   * scala> import cats.syntax.all._, cats.Foldable, cats.data.Const
   * scala> val list = List(1,2,3,4)
   * scala> Foldable[List].partitionBifold(list)(a => ("value " + a.toString(), if (a % 2 == 0) -a else a))
   * res0: (List[String], List[Int]) = (List(value 1, value 2, value 3, value 4),List(1, -2, 3, -4))
   * scala> Foldable[List].partitionBifold(list)(a => Const[Int, Nothing with Any](a))
   * res1: (List[Int], List[Nothing with Any]) = (List(1, 2, 3, 4),List())
   * }}}
   */

  def partitionBifold[H[_, _], A, B, C](
    fa: F[A]
  )(f: A => H[B, C])(implicit A: Alternative[F], H: Bifoldable[H]): (F[B], F[C]) = {
    import cats.instances.tuple._

    implicit val mb: Monoid[F[B]] = A.algebra[B]
    implicit val mc: Monoid[F[C]] = A.algebra[C]

    foldMap[A, (F[B], F[C])](fa)(a =>
      H.bifoldMap[B, C, (F[B], F[C])](f(a))(b => (A.pure(b), A.empty[C]), c => (A.empty[B], A.pure(c)))
    )
  }

  /**
   * Separate this Foldable into a Tuple by an effectful separating function `A => G[H[B, C]]` for some `Bifoldable[H]`
   * Equivalent to `Traverse#traverse` over `Alternative#separate`
   *
   * {{{
   * scala> import cats.syntax.all._, cats.Foldable, cats.data.Const
   * scala> val list = List(1,2,3,4)
   * `Const`'s second parameter is never instantiated, so we can use an impossible type:
   * scala> Foldable[List].partitionBifoldM(list)(a => Option(Const[Int, Nothing with Any](a)))
   * res0: Option[(List[Int], List[Nothing with Any])] = Some((List(1, 2, 3, 4),List()))
   * }}}
   */

  def partitionBifoldM[G[_], H[_, _], A, B, C](
    fa: F[A]
  )(f: A => G[H[B, C]])(implicit A: Alternative[F], M: Monad[G], H: Bifoldable[H]): G[(F[B], F[C])] = {
    import cats.instances.tuple._

    implicit val mb: Monoid[F[B]] = A.algebra[B]
    implicit val mc: Monoid[F[C]] = A.algebra[C]

    foldMapM[G, A, (F[B], F[C])](fa)(a =>
      M.map(f(a)) {
        H.bifoldMap[B, C, (F[B], F[C])](_)(b => (A.pure(b), A.empty[C]), c => (A.empty[B], A.pure(c)))
      }
    )
  }

  /**
   * Separate this Foldable into a Tuple by an effectful separating function `A => G[Either[B, C]]`
   * Equivalent to `Traverse#traverse` over `Alternative#separate`
   *
   * {{{
   * scala> import cats.syntax.all._, cats.Foldable, cats.Eval
   * scala> val list = List(1,2,3,4)
   * scala> val partitioned1 = Foldable[List].partitionEitherM(list)(a => if (a % 2 == 0) Eval.now(Either.left[String, Int](a.toString)) else Eval.now(Either.right[String, Int](a)))
   * Since `Eval.now` yields a lazy computation, we need to force it to inspect the result:
   * scala> partitioned1.value
   * res0: (List[String], List[Int]) = (List(2, 4),List(1, 3))
   * scala> val partitioned2 = Foldable[List].partitionEitherM(list)(a => Eval.later(Either.right(a * 4)))
   * scala> partitioned2.value
   * res1: (List[Nothing], List[Int]) = (List(),List(4, 8, 12, 16))
   * }}}
   */

  def partitionEitherM[G[_], A, B, C](
    fa: F[A]
  )(f: A => G[Either[B, C]])(implicit A: Alternative[F], M: Monad[G]): G[(F[B], F[C])] = {
    import cats.instances.either._
    partitionBifoldM[G, Either, A, B, C](fa)(f)(A, M, Bifoldable[Either])
  }
}

object Foldable {
  private val sentinel: Function1[Any, Any] = new scala.runtime.AbstractFunction1[Any, Any] {
    def apply(a: Any): Any = this
  }

  def iterateRight[A, B](iterable: Iterable[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
    def loop(it: Iterator[A]): Eval[B] =
      Eval.defer(if (it.hasNext) f(it.next(), loop(it)) else lb)

    Eval.always(iterable.iterator).flatMap(loop)
  }

  def iterateRightDefer[G[_]: Defer, A, B](iterable: Iterable[A], lb: G[B])(f: (A, G[B]) => G[B]): G[B] = {
    def loop(it: Iterator[A]): G[B] =
      Defer[G].defer(if (it.hasNext) f(it.next(), Defer[G].defer(loop(it))) else Defer[G].defer(lb))

    Defer[G].defer(loop(iterable.iterator))
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
      def uncons: Option[(Nothing, Eval[Source[Nothing]])] = None
    }

    def cons[A](a: A, src: Eval[Source[A]]): Source[A] =
      new Source[A] {
        def uncons: Option[(A, Eval[Source[A]])] = Some((a, src))
      }

    def fromFoldable[F[_], A](fa: F[A])(implicit F: Foldable[F]): Source[A] =
      F.foldRight[A, Source[A]](fa, Now(Empty))((a, evalSrc) => Later(cons(a, evalSrc))).value
  }

  /**
   * Summon an instance of [[Foldable]] for `F`.
   */
  @inline def apply[F[_]](implicit instance: Foldable[F]): Foldable[F] = instance

  @deprecated("Use cats.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllFoldableOps[F[_], A](target: F[A])(implicit tc: Foldable[F]): AllOps[F, A] {
      type TypeClassType = Foldable[F]
    } =
      new AllOps[F, A] {
        type TypeClassType = Foldable[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  trait Ops[F[_], A] extends Serializable {
    type TypeClassType <: Foldable[F]
    def self: F[A]
    val typeClassInstance: TypeClassType
    def foldLeft[B](b: B)(f: (B, A) => B): B = typeClassInstance.foldLeft[A, B](self, b)(f)
    def foldRight[B](lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = typeClassInstance.foldRight[A, B](self, lb)(f)
    def foldRightDefer[G[_], B](gb: G[B])(fn: (A, G[B]) => G[B])(implicit ev$1: Defer[G]): G[B] =
      typeClassInstance.foldRightDefer[G, A, B](self, gb)(fn)
    def reduceLeftToOption[B](f: A => B)(g: (B, A) => B): Option[B] =
      typeClassInstance.reduceLeftToOption[A, B](self)(f)(g)
    def reduceRightToOption[B](f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[Option[B]] =
      typeClassInstance.reduceRightToOption[A, B](self)(f)(g)
    def reduceLeftOption(f: (A, A) => A): Option[A] = typeClassInstance.reduceLeftOption[A](self)(f)
    def reduceRightOption(f: (A, Eval[A]) => Eval[A]): Eval[Option[A]] = typeClassInstance.reduceRightOption[A](self)(f)
    def minimumOption(implicit A: Order[A]): Option[A] = typeClassInstance.minimumOption[A](self)(A)
    def maximumOption(implicit A: Order[A]): Option[A] = typeClassInstance.maximumOption[A](self)(A)
    def minimumByOption[B](f: A => B)(implicit ev$1: Order[B]): Option[A] =
      typeClassInstance.minimumByOption[A, B](self)(f)
    def maximumByOption[B](f: A => B)(implicit ev$1: Order[B]): Option[A] =
      typeClassInstance.maximumByOption[A, B](self)(f)
    def minimumList(implicit A: Order[A]): List[A] = typeClassInstance.minimumList[A](self)(A)
    def maximumList(implicit A: Order[A]): List[A] = typeClassInstance.maximumList[A](self)(A)
    def minimumByList[B](f: A => B)(implicit ev$1: Order[B]): List[A] = typeClassInstance.minimumByList[A, B](self)(f)
    def maximumByList[B](f: A => B)(implicit ev$1: Order[B]): List[A] = typeClassInstance.maximumByList[A, B](self)(f)
    def get(idx: Long): Option[A] = typeClassInstance.get[A](self)(idx)
    def collectFirst[B](pf: PartialFunction[A, B]): Option[B] = typeClassInstance.collectFirst[A, B](self)(pf)
    def collectFirstSome[B](f: A => Option[B]): Option[B] = typeClassInstance.collectFirstSome[A, B](self)(f)
    def collectFoldSome[B](f: A => Option[B])(implicit B: Monoid[B]): B =
      typeClassInstance.collectFoldSome[A, B](self)(f)(B)
    def fold(implicit A: Monoid[A]): A = typeClassInstance.fold[A](self)(A)
    def sumAll(implicit A: Numeric[A]): A = typeClassInstance.sumAll[A](self)
    def productAll(implicit A: Numeric[A]): A = typeClassInstance.productAll[A](self)
    def combineAll(implicit ev$1: Monoid[A]): A = typeClassInstance.combineAll[A](self)
    def combineAllOption(implicit ev: Semigroup[A]): Option[A] = typeClassInstance.combineAllOption[A](self)(ev)
    def toIterable: Iterable[A] = typeClassInstance.toIterable[A](self)
    def foldMap[B](f: A => B)(implicit B: Monoid[B]): B = typeClassInstance.foldMap[A, B](self)(f)(B)
    def foldM[G[_], B](z: B)(f: (B, A) => G[B])(implicit G: Monad[G]): G[B] =
      typeClassInstance.foldM[G, A, B](self, z)(f)(G)
    final def foldLeftM[G[_], B](z: B)(f: (B, A) => G[B])(implicit G: Monad[G]): G[B] =
      typeClassInstance.foldLeftM[G, A, B](self, z)(f)(G)
    def foldMapM[G[_], B](f: A => G[B])(implicit G: Monad[G], B: Monoid[B]): G[B] =
      typeClassInstance.foldMapM[G, A, B](self)(f)(G, B)
    def foldMapA[G[_], B](f: A => G[B])(implicit G: Applicative[G], B: Monoid[B]): G[B] =
      typeClassInstance.foldMapA[G, A, B](self)(f)(G, B)
    def traverse_[G[_], B](f: A => G[B])(implicit G: Applicative[G]): G[Unit] =
      typeClassInstance.traverse_[G, A, B](self)(f)(G)
    def sequence_[G[_], B](implicit ev$1: A <:< G[B], ev$2: Applicative[G]): G[Unit] =
      typeClassInstance.sequence_[G, B](self.asInstanceOf[F[G[B]]])
    def foldK[G[_], B](implicit ev$1: A <:< G[B], G: MonoidK[G]): G[B] =
      typeClassInstance.foldK[G, B](self.asInstanceOf[F[G[B]]])(G)
    def find(f: A => Boolean): Option[A] = typeClassInstance.find[A](self)(f)
    def existsM[G[_]](p: A => G[Boolean])(implicit G: Monad[G]): G[Boolean] =
      typeClassInstance.existsM[G, A](self)(p)(G)
    def forallM[G[_]](p: A => G[Boolean])(implicit G: Monad[G]): G[Boolean] =
      typeClassInstance.forallM[G, A](self)(p)(G)
    def toList: List[A] = typeClassInstance.toList[A](self)
    def partitionEither[B, C](f: A => Either[B, C])(implicit A: Alternative[F]): (F[B], F[C]) =
      typeClassInstance.partitionEither[A, B, C](self)(f)(A)
    def filter_(p: A => Boolean): List[A] = typeClassInstance.filter_[A](self)(p)
    def takeWhile_(p: A => Boolean): List[A] = typeClassInstance.takeWhile_[A](self)(p)
    def dropWhile_(p: A => Boolean): List[A] = typeClassInstance.dropWhile_[A](self)(p)
    def intercalate(a: A)(implicit A: Monoid[A]): A = typeClassInstance.intercalate[A](self, a)(A)
  }
  trait AllOps[F[_], A] extends Ops[F, A] with UnorderedFoldable.AllOps[F, A] {
    type TypeClassType <: Foldable[F]
  }
  trait ToFoldableOps extends Serializable {
    implicit def toFoldableOps[F[_], A](target: F[A])(implicit tc: Foldable[F]): Ops[F, A] {
      type TypeClassType = Foldable[F]
    } =
      new Ops[F, A] {
        type TypeClassType = Foldable[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToFoldableOps

}
