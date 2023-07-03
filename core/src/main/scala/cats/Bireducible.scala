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

import cats.data.Ior

trait Bireducible[F[_, _]] extends Bifoldable[F] { self =>

  /**
   * Left-reduce `F` by applying `ma` and `mb` to the "initial elements" of `fab` and combine them with
   * every other value using the given functions `mca` and `mcb`.
   * 
   * This method has to be implemented.
   */
  def bireduceLeftTo[A, B, C](fab: F[A, B])(
    ma: A => C,
    mb: B => C
  )(
    mca: (C, A) => C,
    mcb: (C, B) => C
  ): C

  def bireduceRightTo[A, B, C](fab: F[A, B])(
    ma: A => Eval[C],
    mb: B => Eval[C]
  )(
    mac: (A, Eval[C]) => Eval[C],
    mbc: (B, Eval[C]) => Eval[C]
  ): Eval[C]

  def bireduceLeft[A, B](fab: F[A, B])(ma: (A, A) => A, mb: (B, B) => B): A Ior B =
    Bireducible.bireduceLeft(fab)(ma, mb)(self)

  def bireduceRight[A, B](fab: F[A, B])(
    ma: (A, Eval[A]) => Eval[A],
    mb: (B, Eval[B]) => Eval[B]
  ): Eval[A Ior B] =
    Bireducible.bireduceRight(fab)(ma, mb)(self)

  /**
   * Collapse the structure by mapping each element to an element of a type that has a [[cats.Semigroup]]
   */
  def bireduceMap[A, B, C](fab: F[A, B])(ma: A => C, mb: B => C)(implicit C: Semigroup[C]): C =
    Bireducible.bireduceMap(fab)(ma, mb)(self, C)

  def bireduce[A, B](fab: F[A, B])(implicit A: Semigroup[A], B: Semigroup[B]): A Ior B =
    Bireducible.bireduce(fab)(self, A, B)

  def compose[G[_, _]](implicit ev: Bireducible[G]): Bireducible[λ[(α, β) => F[G[α, β], G[α, β]]]] =
    new ComposedBireducible[F, G] {
      override val F = self
      override val G = ev
    }
}

object Bireducible {

  /** 
   * Summon an instance of [[Bireducible]].
   */
  @inline def apply[F[_, _]](implicit F: Bireducible[F]): Bireducible[F] = F

  /** Default implementation for [[cats.Bireducible#bireduceLeft]] based on [[cats.Bireducible#bireduceLeftTo]]. */
  private[cats] def bireduceLeft[F[_, _], A, B](fab: F[A, B])(
    ma: (A, A) => A,
    mb: (B, B) => B
  )(implicit
    F: Bireducible[F]
  ): A Ior B =
    F.bireduceLeftTo(fab)(Ior.left, Ior.right)(
      (c, a) => c.addLeft(a)(ma(_, _)),
      (c, b) => c.addRight(b)(mb(_, _))
    )

  /** Default implementation for [[cats.Bireducible#bireduceRight]] based on [[cats.Bireducible#bireduceRightTo]]. */
  private[cats] def bireduceRight[F[_, _], A, B](fab: F[A, B])(
    ma: (A, Eval[A]) => Eval[A],
    mb: (B, Eval[B]) => Eval[B]
  )(implicit
    F: Bireducible[F]
  ): Eval[A Ior B] =
    F.bireduceRightTo(fab)(
      { a => Eval.now(Ior.left(a)) },
      { b => Eval.now(Ior.right(b)) }
    )(
      { (a, ec) =>
        ec.flatMap {
          case Ior.Left(aa)     => ma(a, Eval.now(aa)).map(Ior.left)
          case Ior.Right(bb)    => Eval.now(Ior.both(a, bb))
          case Ior.Both(aa, bb) => ma(a, Eval.now(aa)).map(Ior.both(_, bb))
        }
      },
      { (b, ec) =>
        ec.flatMap {
          case Ior.Left(aa)     => Eval.now(Ior.Both(aa, b))
          case Ior.Right(bb)    => mb(b, Eval.now(bb)).map(Ior.right)
          case Ior.Both(aa, bb) => mb(b, Eval.now(bb)).map(Ior.both(aa, _))
        }
      }
    )

  /** Default implementation for [[cats.Bireducible#bireduceMap]] based on [[cats.Bireducible#bireduceLeftTo]]. */
  private[cats] def bireduceMap[F[A, B], A, B, C](fab: F[A, B])(
    ma: A => C,
    mb: B => C
  )(implicit
    F: Bireducible[F],
    C: Semigroup[C]
  ): C =
    F.bireduceLeftTo(fab)(ma, mb)(
      (c: C, a: A) => C.combine(c, ma(a)),
      (c: C, b: B) => C.combine(c, mb(b))
    )

  /** Default implementation for [[cats.Bireducible#bireduce]] based on [[cats.Bireducible#bireduceLeft]]. */
  private[cats] def bireduce[F[_, _], A, B](fab: F[A, B])(implicit
    F: Bireducible[F],
    A: Semigroup[A],
    B: Semigroup[B]
  ): A Ior B =
    F.bireduceLeft(fab)(A.combine, B.combine)
}

private[cats] trait ComposedBireducible[F[_, _], G[_, _]]
    extends Bireducible[λ[(α, β) => F[G[α, β], G[α, β]]]]
    with ComposedBifoldable[F, G] {

  implicit def F: Bireducible[F]
  implicit def G: Bireducible[G]

  override def bireduceLeftTo[A, B, C](fgab: F[G[A, B], G[A, B]])(
    ma: A => C,
    mb: B => C
  )(
    mca: (C, A) => C,
    mcb: (C, B) => C
  ): C = {
    def bireduceG(gab: G[A, B]): C = G.bireduceLeftTo(gab)(ma, mb)(mca, mcb)
    def bifoldG(c: C, gab: G[A, B]): C = G.bifoldLeft(gab, c)(mca, mcb)

    F.bireduceLeftTo[G[A, B], G[A, B], C](fgab)(bireduceG, bireduceG)(bifoldG, bifoldG)
  }

  override def bireduceRightTo[A, B, C](fgab: F[G[A, B], G[A, B]])(
    ma: A => Eval[C],
    mb: B => Eval[C]
  )(
    mac: (A, Eval[C]) => Eval[C],
    mbc: (B, Eval[C]) => Eval[C]
  ): Eval[C] = {
    def bireduceG(gab: G[A, B]): Eval[C] = G.bireduceRightTo(gab)(ma, mb)(mac, mbc)
    def bifoldG(gab: G[A, B], c: Eval[C]): Eval[C] = G.bifoldRight(gab, c)(mac, mbc)

    F.bireduceRightTo(fgab)(bireduceG, bireduceG)(bifoldG, bifoldG)
  }
}
