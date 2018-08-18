package cats
package syntax

trait FoldableSyntax extends Foldable.ToFoldableOps with UnorderedFoldable.ToUnorderedFoldableOps {
  implicit final def catsSyntaxNestedFoldable[F[_]: Foldable, G[_], A](fga: F[G[A]]): NestedFoldableOps[F, G, A] =
    new NestedFoldableOps[F, G, A](fga)

  implicit final def catsSyntaxFoldOps[F[_]: Foldable, A](fa: F[A]): FoldableOps[F, A] =
    new FoldableOps[F, A](fa)
}

final class NestedFoldableOps[F[_], G[_], A](val fga: F[G[A]]) extends AnyVal {
  def sequence_(implicit F: Foldable[F], G: Applicative[G]): G[Unit] = F.sequence_(fga)

  /**
   * @see [[Foldable.foldK]].
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   *
   * scala> val l: List[Set[Int]] = List(Set(1, 2), Set(2, 3), Set(3, 4))
   * scala> l.foldK
   * res0: Set[Int] = Set(1, 2, 3, 4)
   * }}}
   */
  def foldK(implicit F: Foldable[F], G: MonoidK[G]): G[A] = F.foldK(fga)
}

final class FoldableOps[F[_], A](val fa: F[A]) extends AnyVal {
  def foldl[B](b: B)(f: (B, A) => B)(implicit F: Foldable[F]): B =
    F.foldLeft(fa, b)(f)

  def foldr[B](b: Eval[B])(f: (A, Eval[B]) => Eval[B])(implicit F: Foldable[F]): Eval[B] =
    F.foldRight(fa, b)(f)

   /**
   * test if `F[A]` contains an `A`, named contains_ to avoid conflict with existing contains which uses universal equality
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   *
   * scala> val l: List[Int] = List(1, 2, 3, 4)
   * scala> l.contains_(1)
   * res0: Boolean = true
   * scala> l.contains_(5)
   * res1: Boolean = false
   * }}}
   */
  def contains_(v: A)(implicit ev: Eq[A], F: Foldable[F]): Boolean =
    F.exists(fa)(ev.eqv(_, v))

   /**
   * Intercalate with a prefix and a suffix
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   *
   * scala> val l: List[String] = List("1", "2", "3")
   * scala> l.foldSmash("List(", ",", ")")
   * res0: String = List(1,2,3)
   * }}}
   */
  def foldSmash(prefix: A, delim: A, suffix: A)(implicit A: Monoid[A], F: Foldable[F]): A =
    A.combine(prefix, A.combine(F.intercalate(fa, delim), suffix))

   /**
   * Make a string using `Show`, named as `mkString_` to avoid conflict
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
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
    val b = F.foldLeft(fa, new StringBuilder){ (builder, a) =>
      builder append A.show(a) append delim
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
    * {{{
    * scala> import cats.implicits._
    * scala> def parseInt(s: String): Either[String, Int] = Either.catchOnly[NumberFormatException](s.toInt).leftMap(_.getMessage)
    * scala> val keys1 = List("1", "2", "4", "5")
    * scala> val map1 = Map(4 -> "Four", 5 -> "Five")
    * scala> keys1.collectFirstSomeM(parseInt(_) map map1.get)
    * res1: scala.util.Either[String,Option[String]] = Right(Some(Four))
    * scala> val map2 = Map(6 -> "Six", 7 -> "Seven")
    * scala> keys1.collectFirstSomeM(parseInt(_) map map2.get)
    * res2: scala.util.Either[String,Option[String]] = Right(None)
    * scala> val keys2 = List("1", "x", "4", "5")
    * scala> keys2.collectFirstSomeM(parseInt(_) map map1.get)
    * res3: scala.util.Either[String,Option[String]] = Left(For input string: "x")
    * scala> val keys3 = List("1", "2", "4", "x")
    * scala> keys3.collectFirstSomeM(parseInt(_) map map1.get)
    * res4: scala.util.Either[String,Option[String]] = Right(Some(Four))
    * }}}
    */
  def collectFirstSomeM[G[_], B](f: A => G[Option[B]])(implicit F: Foldable[F], G: Monad[G]): G[Option[B]] =
    F.foldRight(fa, Eval.now(G.pure(Option.empty[B])))((a, lb) =>
      Eval.now(G.flatMap(f(a)) {
        case None => lb.value
        case s => G.pure(s)
      })
    ).value

  /**
    * Find the first element matching the effectful predicate, if one exists.
    *
    * If there are no elements, the result is `None`. `findM` short-circuits,
    * i.e. once an element is found, no further effects are produced.
    *
    * For example:
    * {{{
    * scala> import cats.implicits._
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
    F.foldRight(fa, Eval.now(G.pure(Option.empty[A])))((a, lb) =>
      Eval.now(G.flatMap(p(a))(if (_) G.pure(Some(a)) else lb.value))
    ).value
}
