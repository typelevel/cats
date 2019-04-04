package cats
package syntax

trait FoldableSyntax extends Foldable.ToFoldableOps with UnorderedFoldable.ToUnorderedFoldableOps {
  implicit final def catsSyntaxNestedFoldable[F[_]: Foldable, G[_], A](fga: F[G[A]]): NestedFoldableOps[F, G, A] =
    new NestedFoldableOps[F, G, A](fga)

  implicit final def catsSyntaxFoldOps[F[_]: Foldable, A](fa: F[A]): FoldableOps[F, A] =
    new FoldableOps[F, A](fa)
}

trait FoldableSyntaxBinCompat0 {
  implicit final def catsSyntaxFoldableOps0[F[_], A](fa: F[A]): FoldableOps0[F, A] =
    new FoldableOps0[F, A](fa)
}

trait FoldableSyntaxBinCompat1 {
  implicit final def catsSyntaxFoldableBinCompat0[F[_]](fa: Foldable[F]): FoldableOps1[F] =
    new FoldableOps1(fa)
}

final class NestedFoldableOps[F[_], G[_], A](private val fga: F[G[A]]) extends AnyVal {
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

final class FoldableOps[F[_], A](private val fa: F[A]) extends AnyVal {
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
   * Make a string using `Show`, prefix, delimiter, and suffix.
   *
   * Named as `mkString_` to avoid conflict.
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
   * scala> import cats.implicits._
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
    G.tailRecM(Foldable.Source.fromFoldable(fa))(_.uncons match {
      case Some((a, src)) =>
        G.map(f(a)) {
          case None => Left(src.value)
          case s    => Right(s)
        }
      case None => G.pure(Right(None))
    })

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
    G.tailRecM(Foldable.Source.fromFoldable(fa))(_.uncons match {
      case Some((a, src)) => G.map(p(a))(if (_) Right(Some(a)) else Left(src.value))
      case None           => G.pure(Right(None))
    })

  /**
   * Tear down a subset of this structure using a `PartialFunction`.
   *{{{
   * scala> import cats.implicits._
   * scala> val xs = List(1, 2, 3, 4)
   * scala> xs.collectFold { case n if n % 2 == 0 => n }
   * res0: Int = 6
   *}}}
   */
  def collectFold[M](f: PartialFunction[A, M])(implicit F: Foldable[F], M: Monoid[M]): M =
    F.foldLeft(fa, M.empty)((acc, a) ⇒ M.combine(acc, f.applyOrElse(a, (_: A) ⇒ M.empty)))

  /**
   * Tear down a subset of this structure using a `A => Option[M]`.
   *{{{
   * scala> import cats.implicits._
   * scala> val xs = List(1, 2, 3, 4)
   * scala> def f(n: Int): Option[Int] = if (n % 2 == 0) Some(n) else None
   * scala> xs.collectSomeFold(f)
   * res0: Int = 6
   *}}}
   */
  def collectSomeFold[M](f: A ⇒ Option[M])(implicit F: Foldable[F], M: Monoid[M]): M =
    F.foldLeft(fa, M.empty)(
      (acc, a) ⇒
        f(a) match {
          case Some(x) ⇒ M.combine(acc, x)
          case None ⇒ acc
      }
    )
}

final class FoldableOps0[F[_], A](private val fa: F[A]) extends AnyVal {

  /**
   * Make a string using `Show` and delimiter.
   *
   * Named as `mkString_` to avoid conflict.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
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
   * scala> import cats._, cats.implicits._
   * scala> val f: Int => Endo[String] = i => (s => s + i)
   * scala> val x: Endo[String] = List(1, 2, 3).foldMapK(f)
   * scala> val a = x("foo")
   * a: String = "foo321"
   * }}}
   * */
  def foldMapK[G[_], B](f: A => G[B])(implicit F: Foldable[F], G: MonoidK[G]): G[B] =
    F.foldMap(fa)(f)(G.algebra)

  /**
   * Separate this Foldable into a Tuple by an effectful separating function `A => H[B, C]` for some `Bifoldable[H]`
   * Equivalent to `Functor#map` over `Alternative#separate`
   *
   * {{{
   * scala> import cats.implicits._, cats.data.Const
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
  )(implicit A: Alternative[F], F: Foldable[F], H: Bifoldable[H]): (F[B], F[C]) = {
    import cats.syntax.foldable._
    F.partitionBifold[H, A, B, C](fa)(f)(A, H)
  }

  /**
   * Separate this Foldable into a Tuple by an effectful separating function `A => G[H[B, C]]` for some `Bifoldable[H]`
   * Equivalent to `Traverse#traverse` over `Alternative#separate`
   *
   * {{{
   * scala> import cats.implicits._, cats.data.Const
   * scala> val list = List(1,2,3,4)
   * `Const`'s second parameter is never instantiated, so we can use an impossible type:
   * scala> list.partitionBifoldM(a => Option(Const[Int, Nothing with Any](a)))
   * res0: Option[(List[Int], List[Nothing with Any])] = Some((List(1, 2, 3, 4),List()))
   * }}}
   */
  def partitionBifoldM[G[_], H[_, _], B, C](
    f: A => G[H[B, C]]
  )(implicit A: Alternative[F], F: Foldable[F], M: Monad[G], H: Bifoldable[H]): G[(F[B], F[C])] = {
    import cats.syntax.foldable._
    F.partitionBifoldM[G, H, A, B, C](fa)(f)(A, M, H)
  }

  /**
   * Separate this Foldable into a Tuple by an effectful separating function `A => G[Either[B, C]]`
   * Equivalent to `Traverse#traverse` over `Alternative#separate`
   *
   * {{{
   * scala> import cats.implicits._, cats.Eval
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
  )(implicit A: Alternative[F], F: Foldable[F], M: Monad[G]): G[(F[B], F[C])] = {
    import cats.syntax.foldable._
    F.partitionEitherM[G, A, B, C](fa)(f)(A, M)
  }

  /**
   * Right associative lazy fold on `F` using the folding function 'f'
   * provided that `G[_]` has a `Defer[G]` instance in scope.
   *
   * For more detailed information about how this method works see the
   * documentation for `Defer[F]`.
   *
   * Example:
   * {{{
   * scala> import cats.Eval, cats.implicits._
   * scala> val fa = Option(1)
   *
   * Folding by addition to zero:
   * With syntax extensions, we can write the same thing like this:
   * scala> val folded2 = fa.foldRightDefer(Eval.now(0))((n, a) => a.map(_ + n))
   * scala> folded2.value
   * res1: Int = 1
   * }}}
   */
  def foldRightDefer[G[_]: Defer, B](gb: G[B])(fn: (A, G[B]) => G[B])(implicit F: Foldable[F]): G[B] = {
    import cats.syntax.foldable._
    F.foldRightDefer(fa, gb)(fn)
  }

}

final class FoldableOps1[F[_]](private val F: Foldable[F]) extends AnyVal {

  /**
   * Separate this Foldable into a Tuple by a separating function `A => H[B, C]` for some `Bifoldable[H]`
   * Equivalent to `Functor#map` and then `Alternative#separate`.
   *
   * {{{
   * scala> import cats.implicits._, cats.Foldable, cats.data.Const
   * scala> val list = List(1,2,3,4)
   * scala> Foldable[List].partitionBifold(list)(a => ("value " + a.toString(), if (a % 2 == 0) -a else a))
   * res0: (List[String], List[Int]) = (List(value 1, value 2, value 3, value 4),List(1, -2, 3, -4))
   * scala> Foldable[List].partitionBifold(list)(a => Const[Int, Nothing with Any](a))
   * res1: (List[Int], List[Nothing with Any]) = (List(1, 2, 3, 4),List())
   * }}}
   */
  def partitionBifold[H[_, _], A, B, C](fa: F[A])(f: A => H[B, C])(implicit A: Alternative[F],
                                                                   H: Bifoldable[H]): (F[B], F[C]) = {
    import cats.instances.tuple._

    implicit val mb: Monoid[F[B]] = A.algebra[B]
    implicit val mc: Monoid[F[C]] = A.algebra[C]

    F.foldMap[A, (F[B], F[C])](fa)(
      a => H.bifoldMap[B, C, (F[B], F[C])](f(a))(b => (A.pure(b), A.empty[C]), c => (A.empty[B], A.pure(c)))
    )
  }

  /**
   * Separate this Foldable into a Tuple by an effectful separating function `A => G[H[B, C]]` for some `Bifoldable[H]`
   * Equivalent to `Traverse#traverse` over `Alternative#separate`
   *
   * {{{
   * scala> import cats.implicits._, cats.Foldable, cats.data.Const
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

    F.foldMapM[G, A, (F[B], F[C])](fa)(
      a =>
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
   * scala> import cats.implicits._, cats.Foldable, cats.Eval
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
  def partitionEitherM[G[_], A, B, C](fa: F[A])(f: A => G[Either[B, C]])(implicit A: Alternative[F],
                                                                         M: Monad[G]): G[(F[B], F[C])] = {
    import cats.instances.either._
    partitionBifoldM[G, Either, A, B, C](fa)(f)(A, M, Bifoldable[Either])
  }

  /**
   * Right associative lazy fold on `F` using the folding function 'f'
   * provided that `G[_]` has a `Defer[G]` instance in scope.
   *
   * For more detailed information about how this method works see the
   * documentation for `Defer[F]`.
   *
   * Example:
   * {{{
   * scala> import cats.Foldable, cats.Eval, cats.implicits._
   * scala> val fa = Option(1)
   *
   * Folding by addition to zero:
   * scala> val folded1 = Foldable[Option].foldRightDefer(fa, Eval.now(0))((n, a) => a.map(_ + n))
   * Since `foldRightDefer` yields a lazy computation, we need to force it to inspect the result:
   * scala> folded1.value
   * res0: Int = 1
   * }}}
   */
  def foldRightDefer[G[_]: Defer, A, B](fa: F[A], gb: G[B])(fn: (A, G[B]) => G[B]): G[B] =
    Defer[G].defer(
      F.foldLeft(fa, (z: G[B]) => z) { (acc, elem) => z =>
        Defer[G].defer(acc(fn(elem, z)))
      }(gb)
    )
}
