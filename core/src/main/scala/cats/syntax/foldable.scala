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
package syntax

trait FoldableSyntax extends Foldable.ToFoldableOps with UnorderedFoldable.ToUnorderedFoldableOps {

  implicit final def catsSyntaxNestedFoldable[F[_]: Foldable, G[_], A](fga: F[G[A]]): NestedFoldableOps[F, G, A] =
    new NestedFoldableOps[F, G, A](fga)

  implicit final def catsSyntaxFoldOps[F[_], A](fa: F[A]): FoldableOps[F, A] =
    new FoldableOps[F, A](fa)

  @deprecated("Use overload without Foldable parameter", "2.9.0")
  final def catsSyntaxFoldOps[F[_], A](fa: F[A], F: Foldable[F]): FoldableOps[F, A] =
    new FoldableOps[F, A](fa)
}

private[syntax] trait FoldableSyntaxBinCompat0 {
  implicit final def catsSyntaxFoldableOps0[F[_], A](fa: F[A]): FoldableOps0[F, A] =
    new FoldableOps0[F, A](fa)
}

private[syntax] trait FoldableSyntaxBinCompat1 {
  @deprecated("Use methods on Foldable", "2.1.0-RC1")
  final def catsSyntaxFoldableBinCompat0[F[_]](fa: Foldable[F]): FoldableOps1[F] =
    new FoldableOps1(fa)
}

final class NestedFoldableOps[F[_], G[_], A](private val fga: F[G[A]]) extends AnyVal {
  def sequence_(implicit F: Foldable[F], G: Applicative[G]): G[Unit] = F.sequence_(fga)

  /**
   * @see [[Foldable.foldK]].
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   *
   * scala> val l: List[Set[Int]] = List(Set(1, 2), Set(2, 3), Set(3, 4))
   * scala> l.foldK
   * res0: Set[Int] = Set(1, 2, 3, 4)
   * }}}
   */
  def foldK(implicit F: Foldable[F], G: MonoidK[G]): G[A] = F.foldK(fga)
}

final class FoldableOps[F[_], A](private val fa: F[A]) extends AnyVal {
  def foldl[B](b: B)(f: (B, A) => B)(implicit F: Foldable[F]): B =
    F.foldLeft(fa, b)(f)

  def foldr[B](b: Eval[B])(f: (A, Eval[B]) => Eval[B])(implicit F: Foldable[F]): Eval[B] =
    F.foldRight(fa, b)(f)

  def foldA[G[_], B](implicit F: Foldable[F], ev: A <:< G[B], G: Applicative[G], B: Monoid[B]): G[B] =
    F.foldA[G, B](fa.asInstanceOf[F[G[B]]])

  private[syntax] def contains_(v: A, eq: Eq[A], F: Foldable[F]): Boolean =
    F.contains_(fa, v)(eq)

  /**
   * Intercalate with a prefix and a suffix
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   *
   * scala> val l: List[String] = List("1", "2", "3")
   * scala> l.foldSmash("List(", ",", ")")
   * res0: String = List(1,2,3)
   * }}}
   */
  def foldSmash(prefix: A, delim: A, suffix: A)(implicit A: Monoid[A], F: Foldable[F]): A =
    A.combine(prefix, A.combine(F.intercalate(fa, delim), suffix))

  /**
   * Make a string using `Show`, prefix, delimiter, and suffix.
   *
   * Named as `mkString_` to avoid conflict.
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   *
   * scala> val l: List[Int] = List(1, 2, 3)
   * scala> l.mkString_("L[", ";", "]")
   * res0: String = L[1;2;3]
   * scala> val el: List[Int] = List()
   * scala> el.mkString_("L[", ";", "]")
   * res1: String = L[]
   * }}}
   */
  def mkString_(prefix: String, delim: String, suffix: String)(implicit A: Show[A], F: Foldable[F]): String = {
    val b = F.foldLeft(fa, new StringBuilder) { (builder, a) =>
      builder.append(A.show(a)).append(delim)
    }
    prefix + {
      if (b.isEmpty)
        ""
      else
        b.toString.dropRight(delim.length)
    } + suffix
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
   * scala> keys1.collectFirstSomeM(parseInt(_) map map1.get)
   * res0: scala.util.Either[String,Option[String]] = Right(Some(Four))
   *
   * scala> val map2 = Map(6 -> "Six", 7 -> "Seven")
   * scala> keys1.collectFirstSomeM(parseInt(_) map map2.get)
   * res1: scala.util.Either[String,Option[String]] = Right(None)
   *
   * scala> val keys2 = List("1", "x", "4", "5")
   * scala> keys2.collectFirstSomeM(parseInt(_) map map1.get)
   * res2: scala.util.Either[String,Option[String]] = Left(For input string: "x")
   *
   * scala> val keys3 = List("1", "2", "4", "x")
   * scala> keys3.collectFirstSomeM(parseInt(_) map map1.get)
   * res3: scala.util.Either[String,Option[String]] = Right(Some(Four))
   * }}}
   */
  def collectFirstSomeM[G[_], B](f: A => G[Option[B]])(implicit F: Foldable[F], G: Monad[G]): G[Option[B]] =
    F.collectFirstSomeM[G, A, B](fa)(f)

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
   * scala> list.findM(n => (n >= 2).asRight[String])
   * res0: Either[String,Option[Int]] = Right(Some(2))
   *
   * scala> list.findM(n => (n > 4).asRight[String])
   * res1: Either[String,Option[Int]] = Right(None)
   *
   * scala> list.findM(n => Either.cond(n < 3, n >= 2, "error"))
   * res2: Either[String,Option[Int]] = Right(Some(2))
   *
   * scala> list.findM(n => Either.cond(n < 3, false, "error"))
   * res3: Either[String,Option[Int]] = Left(error)
   * }}}
   */
  def findM[G[_]](p: A => G[Boolean])(implicit F: Foldable[F], G: Monad[G]): G[Option[A]] =
    F.findM[G, A](fa)(p)

  /**
   * Tear down a subset of this structure using a `PartialFunction`.
   * {{{
   * scala> import cats.syntax.all._
   * scala> val xs = List(1, 2, 3, 4)
   * scala> xs.collectFold { case n if n % 2 == 0 => n }
   * res0: Int = 6
   * }}}
   */
  def collectFold[M](f: PartialFunction[A, M])(implicit F: Foldable[F], M: Monoid[M]): M =
    F.collectFold[A, M](fa)(f)

  /**
   * Tear down a subset of this structure using a `A => Option[M]`.
   * {{{
   * scala> import cats.syntax.all._
   * scala> val xs = List(1, 2, 3, 4)
   * scala> def f(n: Int): Option[Int] = if (n % 2 == 0) Some(n) else None
   * scala> xs.collectFoldSome(f)
   * res0: Int = 6
   * }}}
   */
  @deprecated("Use collectFoldSome", "2.1.0-RC1")
  def collectSomeFold[M](f: A => Option[M])(implicit F: Foldable[F], M: Monoid[M]): M =
    F.collectFoldSome[A, M](fa)(f)
}

final class FoldableOps0[F[_], A](private val fa: F[A]) extends AnyVal {

  /**
   * Make a string using `Show` and delimiter.
   *
   * Named as `mkString_` to avoid conflict.
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   *
   * scala> val l: List[Int] = List(1, 2, 3)
   * scala> l.mkString_(",")
   * res0: String = 1,2,3
   * scala> val el: List[Int] = List()
   * scala> el.mkString_(",")
   * res1: String =
   * }}}
   */
  def mkString_(delim: String)(implicit A: Show[A], F: Foldable[F]): String =
    new FoldableOps(fa).mkString_("", delim, "")

  /**
   * Fold implemented by mapping `A` values into `B` in a context `G` and then
   * combining them using the `MonoidK[G]` instance.
   *
   * {{{
   * scala> import cats._, cats.syntax.all._
   * scala> val f: Int => Endo[String] = i => (s => s + i)
   * scala> val x: Endo[String] = List(1, 2, 3).foldMapK(f)
   * scala> val a = x("foo")
   * a: String = "foo321"
   * }}}
   */
  def foldMapK[G[_], B](f: A => G[B])(implicit F: Foldable[F], G: MonoidK[G]): G[B] =
    F.foldMapK(fa)(f)

  /**
   * Separate this Foldable into a Tuple by an effectful separating function `A => H[B, C]` for some `Bifoldable[H]`
   * Equivalent to `Functor#map` over `Alternative#separate`
   *
   * {{{
   * scala> import cats.syntax.all._, cats.data.Const
   * scala> val list = List(1,2,3,4)
   * scala> list.partitionBifold(a => (a, "value " + a.toString))
   * res0: (List[Int], List[String]) = (List(1, 2, 3, 4),List(value 1, value 2, value 3, value 4))
   * `Const`'s second parameter is never instantiated, so we can use an impossible type:
   * scala> list.partitionBifold(a => Const[Int, Nothing with Any](a))
   * res1: (List[Int], List[Nothing with Any]) = (List(1, 2, 3, 4),List())
   * }}}
   */
  def partitionBifold[H[_, _], B, C](
    f: A => H[B, C]
  )(implicit A: Alternative[F], F: Foldable[F], H: Bifoldable[H]): (F[B], F[C]) =
    F.partitionBifold[H, A, B, C](fa)(f)(A, H)

  /**
   * Separate this Foldable into a Tuple by an effectful separating function `A => G[H[B, C]]` for some `Bifoldable[H]`
   * Equivalent to `Traverse#traverse` over `Alternative#separate`
   *
   * {{{
   * scala> import cats.syntax.all._, cats.data.Const
   * scala> val list = List(1,2,3,4)
   * `Const`'s second parameter is never instantiated, so we can use an impossible type:
   * scala> list.partitionBifoldM(a => Option(Const[Int, Nothing with Any](a)))
   * res0: Option[(List[Int], List[Nothing with Any])] = Some((List(1, 2, 3, 4),List()))
   * }}}
   */
  def partitionBifoldM[G[_], H[_, _], B, C](
    f: A => G[H[B, C]]
  )(implicit A: Alternative[F], F: Foldable[F], M: Monad[G], H: Bifoldable[H]): G[(F[B], F[C])] =
    F.partitionBifoldM[G, H, A, B, C](fa)(f)(A, M, H)

  /**
   * Separate this Foldable into a Tuple by an effectful separating function `A => G[Either[B, C]]`
   * Equivalent to `Traverse#traverse` over `Alternative#separate`
   *
   * {{{
   * scala> import cats.syntax.all._, cats.Eval
   * scala> val list = List(1,2,3,4)
   * scala> val partitioned1 = list.partitionEitherM(a => if (a % 2 == 0) Eval.now(Either.left[String, Int](a.toString)) else Eval.now(Either.right[String, Int](a)))
   * Since `Eval.now` yields a lazy computation, we need to force it to inspect the result:
   * scala> partitioned1.value
   * res0: (List[String], List[Int]) = (List(2, 4),List(1, 3))
   * scala> val partitioned2 = list.partitionEitherM(a => Eval.later(Either.right(a * 4)))
   * scala> partitioned2.value
   * res1: (List[Nothing], List[Int]) = (List(),List(4, 8, 12, 16))
   * }}}
   */
  def partitionEitherM[G[_], B, C](
    f: A => G[Either[B, C]]
  )(implicit A: Alternative[F], F: Foldable[F], M: Monad[G]): G[(F[B], F[C])] =
    F.partitionEitherM[G, A, B, C](fa)(f)(A, M)

  def sliding2(implicit F: Foldable[F]): List[(A, A)] =
    F.sliding2(fa)
  def sliding3(implicit F: Foldable[F]): List[(A, A, A)] =
    F.sliding3(fa)
  def sliding4(implicit F: Foldable[F]): List[(A, A, A, A)] =
    F.sliding4(fa)
  def sliding5(implicit F: Foldable[F]): List[(A, A, A, A, A)] =
    F.sliding5(fa)
  def sliding6(implicit F: Foldable[F]): List[(A, A, A, A, A, A)] =
    F.sliding6(fa)
  def sliding7(implicit F: Foldable[F]): List[(A, A, A, A, A, A, A)] =
    F.sliding7(fa)
  def sliding8(implicit F: Foldable[F]): List[(A, A, A, A, A, A, A, A)] =
    F.sliding8(fa)
  def sliding9(implicit F: Foldable[F]): List[(A, A, A, A, A, A, A, A, A)] =
    F.sliding9(fa)
  def sliding10(implicit F: Foldable[F]): List[(A, A, A, A, A, A, A, A, A, A)] =
    F.sliding10(fa)
  def sliding11(implicit F: Foldable[F]): List[(A, A, A, A, A, A, A, A, A, A, A)] =
    F.sliding11(fa)
  def sliding12(implicit F: Foldable[F]): List[(A, A, A, A, A, A, A, A, A, A, A, A)] =
    F.sliding12(fa)
  def sliding13(implicit F: Foldable[F]): List[(A, A, A, A, A, A, A, A, A, A, A, A, A)] =
    F.sliding13(fa)
  def sliding14(implicit F: Foldable[F]): List[(A, A, A, A, A, A, A, A, A, A, A, A, A, A)] =
    F.sliding14(fa)
  def sliding15(implicit F: Foldable[F]): List[(A, A, A, A, A, A, A, A, A, A, A, A, A, A, A)] =
    F.sliding15(fa)
  def sliding16(implicit F: Foldable[F]): List[(A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A)] =
    F.sliding16(fa)
  def sliding17(implicit F: Foldable[F]): List[(A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A)] =
    F.sliding17(fa)
  def sliding18(implicit F: Foldable[F]): List[(A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A)] =
    F.sliding18(fa)
  def sliding19(implicit F: Foldable[F]): List[(A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A)] =
    F.sliding19(fa)
  def sliding20(implicit F: Foldable[F]): List[(A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A)] =
    F.sliding20(fa)
  def sliding21(implicit F: Foldable[F]): List[(A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A)] =
    F.sliding21(fa)
  def sliding22(implicit F: Foldable[F]): List[(A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A)] =
    F.sliding22(fa)
}

@deprecated("Use methods on Foldable", "2.1.0-RC1")
final private[syntax] class FoldableOps1[F[_]](private val F: Foldable[F]) extends AnyVal {

  @deprecated("Use partitionBifold on Foldable", "2.1.0-RC1")
  def partitionBifold[H[_, _], A, B, C](
    fa: F[A]
  )(f: A => H[B, C])(implicit A: Alternative[F], H: Bifoldable[H]): (F[B], F[C]) =
    F.partitionBifold[H, A, B, C](fa)(f)

  @deprecated("Use partitionBifoldM on Foldable", "2.1.0-RC1")
  def partitionBifoldM[G[_], H[_, _], A, B, C](
    fa: F[A]
  )(f: A => G[H[B, C]])(implicit A: Alternative[F], M: Monad[G], H: Bifoldable[H]): G[(F[B], F[C])] =
    F.partitionBifoldM[G, H, A, B, C](fa)(f)

  @deprecated("Use partitionEitherM on Foldable", "2.1.0-RC1")
  def partitionEitherM[G[_], A, B, C](
    fa: F[A]
  )(f: A => G[Either[B, C]])(implicit A: Alternative[F], M: Monad[G]): G[(F[B], F[C])] =
    F.partitionEitherM[G, A, B, C](fa)(f)
}
