package cats
package laws

import cats.arrow.Strong
import cats.syntax.profunctor._
import cats.syntax.strong._

/**
 * Laws that must be obeyed by any `cats.functor.Strong`.
 *
 * See: [[https://arxiv.org/abs/1406.4823 E. Rivas, M. Jaskelioff Notions of Computation as Monoids, Chapter 7]]
 * See: [[http://hackage.haskell.org/package/profunctors/docs/Data-Profunctor-Strong.html Haskell Data.Profunctor.Strong]]
 */
trait StrongLaws[F[_, _]] extends ProfunctorLaws[F] {
  implicit override def F: Strong[F]

  private def swapTuple[X, Y]: Tuple2[X, Y] => Tuple2[Y, X] = _.swap

  /**
   * first' == dimap swap swap . second'
   */
  def firstIsSwappedSecond[A, B, C](fab: F[A, B]): IsEq[F[(A, C), (B, C)]] =
    fab.first[C] <-> fab.second[C].dimap(swapTuple[A, C])(swapTuple[C, B])

  /**
   * second' == dimap swap swap . first'
   */
  def secondIsSwappedFirst[A, B, C](fab: F[A, B]): IsEq[F[(C, A), (C, B)]] =
    fab.second[C] <-> fab.first[C].dimap(swapTuple[C, A])(swapTuple[B, C])

  /**
   * lmap fst == rmap fst . first'
   */
  def lmapEqualsFirstAndThenRmap[A, B, C](fab: F[A, B]): IsEq[F[(A, C), B]] =
    fab.lmap[(A, C)]({ case (a, _) => a }) <-> fab.first[C].rmap[B](_._1)

  /**
   * lmap snd == rmap snd . second'
   */
  def lmapEqualsSecondAndThenRmap[A, B, C](fab: F[A, B]): IsEq[F[(C, A), B]] =
    fab.lmap[(C, A)]({ case (_, b) => b }) <-> fab.second[C].rmap[B](_._2)

  private def mapFirst[X, Y, Z](f: X => Z)(cb: (X, Y)): (Z, Y) = (f(cb._1), cb._2)
  private def mapSecond[X, Y, Z](f: Y => Z)(cb: (X, Y)): (X, Z) = (cb._1, f(cb._2))

  /**
   * lmap (second f) . first == rmap (second f) . first
   */
  def dinaturalityFirst[A, B, C, D](fab: F[A, B], f: C => D): IsEq[F[(A, C), (B, D)]] =
    fab.first[C].rmap(mapSecond(f)) <-> fab.first[D].lmap(mapSecond(f))

  /**
   * lmap (first f) . second == rmap (first f) . second
   */
  def dinaturalitySecond[A, B, C, D](fab: F[A, B], f: C => D): IsEq[F[(C, A), (D, B)]] =
    fab.second[C].rmap(mapFirst(f)) <-> fab.second[D].lmap(mapFirst(f))

  private def assoc[A, B, C]: (((A, B), C)) => (A, (B, C)) = { case ((a, c), d) => (a, (c, d)) }
  private def unassoc[A, B, C]: ((A, (B, C))) => ((A, B), C) = { case (a, (c, d)) => ((a, c), d) }

  /**
   * first' . first' == dimap assoc unassoc . first' where
   *   assoc ((a,b),c) = (a,(b,c))
   *   unassoc (a,(b,c)) = ((a,b),c)
   */
  def firstFirstIsDimap[A, B, C, D](fab: F[A, B]): IsEq[F[((A, C), D), ((B, C), D)]] =
    fab.first[C].first[D] <-> fab.first[(C, D)].dimap[((A, C), D), ((B, C), D)](assoc)(unassoc)

  /**
   * second' . second' == dimap unassoc assoc . second' where
   *   assoc ((a,b),c) = (a,(b,c))
   *   unassoc (a,(b,c)) = ((a,b),c)
   */
  def secondSecondIsDimap[A, B, C, D](fab: F[A, B]): IsEq[F[(D, (C, A)), (D, (C, B))]] =
    fab.second[C].second[D] <-> fab.second[(D, C)].dimap[(D, (C, A)), (D, (C, B))](unassoc)(assoc)
}

object StrongLaws {
  def apply[F[_, _]](implicit ev: Strong[F]): StrongLaws[F] =
    new StrongLaws[F] { def F: Strong[F] = ev }
}
