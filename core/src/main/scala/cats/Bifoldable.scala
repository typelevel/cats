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

/**
 * A type class abstracting over types that give rise to two independent [[cats.Foldable]]s.
 */
trait Bifoldable[F[_, _]] extends Serializable { self =>

  /**
   * Collapse the structure with a left-associative function
   *
   * Example:
   * {{{
   * scala> import cats.Bifoldable
   * scala> val fab = (List(1), 2)
   *
   * Folding by addition to zero:
   * scala> Bifoldable[Tuple2].bifoldLeft(fab, Option(0))((c, a) => c.map(_ + a.head), (c, b) => c.map(_ + b))
   * res0: Option[Int] = Some(3)
   * }}}
   *
   * With syntax extensions, `bifoldLeft` can be used like:
   * {{{
   * scala> import cats.syntax.all._
   * scala> fab.bifoldLeft(Option(0))((c, a) => c.map(_ + a.head), (c, b) => c.map(_ + b))
   * res1: Option[Int] = Some(3)
   * }}}
   */
  def bifoldLeft[A, B, C](fab: F[A, B], c: C)(f: (C, A) => C, g: (C, B) => C): C

  /**
   * Collapse the structure with a right-associative function
   * Right associative lazy bifold on `F` using the folding function 'f' and 'g'.
   *
   * This method evaluates `c` lazily (in some cases it will not be
   * needed), and returns a lazy value. We are using `(_, Eval[C]) =>
   * Eval[C]` to support laziness in a stack-safe way. Chained
   * computation should be performed via .map and .flatMap.
   *
   * For more detailed information about how this method works see the
   * documentation for `Eval[_]`.
   *
   * Example:
   * {{{
   * scala> import cats.Bifoldable
   * scala> val fab = (List(1), 2)
   *
   * Folding by addition to zero:
   * scala> val bifolded1 = Bifoldable[Tuple2].bifoldRight(fab, Eval.now(0))((a, c) => c.map(_ + a.head), (b, c) => c.map(_ + b))
   * scala> bifolded1.value
   * res0: Int = 3
   * }}}
   *
   * With syntax extensions, `bifoldRight` can be used like:
   * {{{
   * scala> import cats.syntax.all._
   * scala> val bifolded2 = fab.bifoldRight(Eval.now(0))((a, c) => c.map(_ + a.head), (b, c) => c.map(_ + b))
   * scala> bifolded2.value
   * res1: Int = 3
   * }}}
   */
  def bifoldRight[A, B, C](fab: F[A, B], c: Eval[C])(f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C]): Eval[C]

  /**
   * Collapse the structure by mapping each element to an element of a type that has a [[cats.Monoid]]
   */
  def bifoldMap[A, B, C](fab: F[A, B])(f: A => C, g: B => C)(implicit C: Monoid[C]): C =
    bifoldLeft(fab, C.empty)(
      (c: C, a: A) => C.combine(c, f(a)),
      (c: C, b: B) => C.combine(c, g(b))
    )

  def compose[G[_, _]](implicit ev: Bifoldable[G]): Bifoldable[λ[(α, β) => F[G[α, β], G[α, β]]]] =
    new ComposedBifoldable[F, G] {
      val F = self
      val G = ev
    }

  def bifold[A, B](fab: F[A, B])(implicit A: Monoid[A], B: Monoid[B]): (A, B) = {
    import cats.instances.tuple._
    bifoldMap(fab)((_, B.empty), (A.empty, _))
  }
}

object Bifoldable extends cats.instances.NTupleBitraverseInstances {
  implicit def catsBitraverseForEither: Bitraverse[Either] = cats.instances.either.catsStdBitraverseForEither

  @deprecated("Use catsStdBitraverseForTuple2 in cats.instances.NTupleBitraverseInstances", "2.4.0")
  def catsBitraverseForTuple2: Bitraverse[Tuple2] = cats.instances.tuple.catsStdBitraverseForTuple2

  /**
   * Summon an instance of [[Bifoldable]] for `F`.
   */
  @inline def apply[F[_, _]](implicit instance: Bifoldable[F]): Bifoldable[F] = instance

  @deprecated("Use cats.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllBifoldableOps[F[_, _], A, B](target: F[A, B])(implicit tc: Bifoldable[F]): AllOps[F, A, B] {
      type TypeClassType = Bifoldable[F]
    } =
      new AllOps[F, A, B] {
        type TypeClassType = Bifoldable[F]
        val self: F[A, B] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  trait Ops[F[_, _], A, B] extends Serializable {
    type TypeClassType <: Bifoldable[F]
    def self: F[A, B]
    val typeClassInstance: TypeClassType
    def bifoldLeft[C](c: C)(f: (C, A) => C, g: (C, B) => C): C = typeClassInstance.bifoldLeft[A, B, C](self, c)(f, g)
    def bifoldRight[C](c: Eval[C])(f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C]): Eval[C] =
      typeClassInstance.bifoldRight[A, B, C](self, c)(f, g)
    def bifoldMap[C](f: A => C, g: B => C)(implicit C: Monoid[C]): C =
      typeClassInstance.bifoldMap[A, B, C](self)(f, g)(C)
    def bifold(implicit A: Monoid[A], B: Monoid[B]): (A, B) = typeClassInstance.bifold[A, B](self)(A, B)
  }
  trait AllOps[F[_, _], A, B] extends Ops[F, A, B]
  trait ToBifoldableOps extends Serializable {
    implicit def toBifoldableOps[F[_, _], A, B](target: F[A, B])(implicit tc: Bifoldable[F]): Ops[F, A, B] {
      type TypeClassType = Bifoldable[F]
    } =
      new Ops[F, A, B] {
        type TypeClassType = Bifoldable[F]
        val self: F[A, B] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToBifoldableOps

}

private[cats] trait ComposedBifoldable[F[_, _], G[_, _]] extends Bifoldable[λ[(α, β) => F[G[α, β], G[α, β]]]] {
  implicit def F: Bifoldable[F]
  implicit def G: Bifoldable[G]

  override def bifoldLeft[A, B, C](fab: F[G[A, B], G[A, B]], c: C)(f: (C, A) => C, g: (C, B) => C): C =
    F.bifoldLeft(fab, c)(
      (c: C, gab: G[A, B]) => G.bifoldLeft(gab, c)(f, g),
      (c: C, gab: G[A, B]) => G.bifoldLeft(gab, c)(f, g)
    )

  override def bifoldRight[A, B, C](fab: F[G[A, B], G[A, B]],
                                    c: Eval[C]
  )(f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C]): Eval[C] =
    F.bifoldRight(fab, c)(
      (gab: G[A, B], c: Eval[C]) => G.bifoldRight(gab, c)(f, g),
      (gab: G[A, B], c: Eval[C]) => G.bifoldRight(gab, c)(f, g)
    )
}
