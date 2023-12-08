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

import cats.data.Chain
import cats.data.State
import cats.data.StateT
import cats.kernel.compat.scalaVersionSpecific._

/**
 * Traverse, also known as Traversable.
 *
 * Traversal over a structure with an effect.
 *
 * Traversing with the [[cats.Id]] effect is equivalent to [[cats.Functor]]#map.
 * Traversing with the [[cats.data.Const]] effect where the first type parameter has
 * a [[cats.Monoid]] instance is equivalent to [[cats.Foldable]]#fold.
 *
 * See: [[https://www.cs.ox.ac.uk/jeremy.gibbons/publications/iterator.pdf The Essence of the Iterator Pattern]]
 */
trait Traverse[F[_]] extends Functor[F] with Foldable[F] with UnorderedTraverse[F] { self =>

  /**
   * Given a function which returns a G effect, thread this effect
   * through the running of this function on all the values in F,
   * returning an F[B] in a G context.
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> def parseInt(s: String): Option[Int] = Either.catchOnly[NumberFormatException](s.toInt).toOption
   * scala> List("1", "2", "3").traverse(parseInt)
   * res0: Option[List[Int]] = Some(List(1, 2, 3))
   * scala> List("1", "two", "3").traverse(parseInt)
   * res1: Option[List[Int]] = None
   * }}}
   */
  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

  /**
   * Given a function which returns a G effect, thread this effect
   * through the running of this function on all the values in F,
   * returning an F[A] in a G context, ignoring the values
   * returned by provided function.
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> import java.io.IOException
   * scala> type IO[A] = Either[IOException, A]
   * scala> def debug(msg: String): IO[Unit] = Right(())
   * scala> List("1", "2", "3").traverseTap(debug)
   * res1: IO[List[String]] = Right(List(1, 2, 3))
   * }}}
   */
  def traverseTap[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[A]] =
    traverse(fa)(a => Applicative[G].as(f(a), a))

  /**
   * A traverse followed by flattening the inner result.
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> def parseInt(s: String): Option[Int] = Either.catchOnly[NumberFormatException](s.toInt).toOption
   * scala> val x = Option(List("1", "two", "3"))
   * scala> x.flatTraverse(_.map(parseInt))
   * res0: List[Option[Int]] = List(Some(1), None, Some(3))
   * }}}
   */
  def flatTraverse[G[_], A, B](fa: F[A])(f: A => G[F[B]])(implicit G: Applicative[G], F: FlatMap[F]): G[F[B]] =
    G.map(traverse(fa)(f))(F.flatten)

  /**
   * Thread all the G effects through the F structure to invert the
   * structure from F[G[A]] to G[F[A]].
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> val x: List[Option[Int]] = List(Some(1), Some(2))
   * scala> val y: List[Option[Int]] = List(None, Some(2))
   * scala> x.sequence
   * res0: Option[List[Int]] = Some(List(1, 2))
   * scala> y.sequence
   * res1: Option[List[Int]] = None
   * }}}
   */
  def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]] =
    traverse(fga)(ga => ga)

  /**
   * Thread all the G effects through the F structure and flatten to invert the
   * structure from F[G[F[A]]] to G[F[A]].
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> val x: List[Option[List[Int]]] = List(Some(List(1, 2)), Some(List(3)))
   * scala> val y: List[Option[List[Int]]] = List(None, Some(List(3)))
   * scala> x.flatSequence
   * res0: Option[List[Int]] = Some(List(1, 2, 3))
   * scala> y.flatSequence
   * res1: Option[List[Int]] = None
   * }}}
   */
  def flatSequence[G[_], A](fgfa: F[G[F[A]]])(implicit G: Applicative[G], F: FlatMap[F]): G[F[A]] =
    G.map(sequence(fgfa))(F.flatten)

  def compose[G[_]: Traverse]: Traverse[λ[α => F[G[α]]]] =
    new ComposedTraverse[F, G] {
      val F = self
      val G = Traverse[G]
    }

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    traverse[Id, A, B](fa)(f)

  /**
   * Akin to [[map]], but allows to keep track of a state value
   * when calling the function.
   */
  def mapAccumulate[S, A, B](init: S, fa: F[A])(f: (S, A) => (S, B)): (S, F[B]) =
    traverse(fa)(a => State(s => f(s, a))).run(init).value

  /**
   * Akin to [[map]], but also provides the value's index in structure
   * F when calling the function.
   */
  def mapWithIndex[A, B](fa: F[A])(f: (A, Int) => B): F[B] =
    mapAccumulate(0, fa)((i, a) => (i + 1) -> f(a, i))._2

  /**
   * Akin to [[traverse]], but also provides the value's index in
   * structure F when calling the function.
   *
   * This performs the traversal in a single pass but requires that
   * effect G is monadic. An applicative traversal can be performed in
   * two passes using [[zipWithIndex]] followed by [[traverse]].
   */
  def traverseWithIndexM[G[_], A, B](fa: F[A])(f: (A, Int) => G[B])(implicit G: Monad[G]): G[F[B]] =
    traverse(fa)(a => StateT((s: Int) => G.map(f(a, s))(b => (s + 1, b)))).runA(0)

  /**
   * Traverses through the structure F, pairing the values with
   * assigned indices.
   *
   * The behavior is consistent with the Scala collection library's
   * `zipWithIndex` for collections such as `List`.
   */
  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapWithIndex(fa)((a, i) => (a, i))

  /**
    * Same as [[traverseWithIndexM]] but the index type is [[Long]] instead of [[Int]].
    */
  def traverseWithLongIndexM[G[_], A, B](fa: F[A])(f: (A, Long) => G[B])(implicit G: Monad[G]): G[F[B]] =
    traverse(fa)(a => StateT((s: Long) => G.map(f(a, s))(b => (s + 1, b)))).runA(0L)

  /**
    * Same as [[mapWithIndex]] but the index type is [[Long]] instead of [[Int]].
    */
  def mapWithLongIndex[A, B](fa: F[A])(f: (A, Long) => B): F[B] =
    traverseWithLongIndexM[cats.Id, A, B](fa)((a, long) => f(a, long))

  /**
    * Same as [[zipWithIndex]] but the index type is [[Long]] instead of [[Int]].
    */
  def zipWithLongIndex[A](fa: F[A]): F[(A, Long)] =
    mapWithLongIndex(fa)((a, long) => (a, long))

  /**
   * If `fa` contains the element at index `idx`, 
   * return the copy of `fa` where the element at `idx` is replaced with `b`. 
   * If there is no element with such an index, return `None`. 
   *
   * The behavior is consistent with the Scala collection library's
   * `updated` for collections such as `List`.
   */
  def updated_[A, B >: A](fa: F[A], idx: Long, b: B): Option[F[B]] = {
    if (idx < 0L)
      None
    else
      mapAccumulate(0L, fa)((i, a) =>
        if (i == idx)
          (i + 1, b)
        else
          (i + 1, a)
      ) match {
        case (i, fb) if i > idx => Some(fb)
        case _                  => None
      }
  }
  override def unorderedTraverse[G[_]: CommutativeApplicative, A, B](sa: F[A])(f: (A) => G[B]): G[F[B]] =
    traverse(sa)(f)

  override def unorderedSequence[G[_]: CommutativeApplicative, A](fga: F[G[A]]): G[F[A]] =
    sequence(fga)
}

object Traverse {

  /**
   * Summon an instance of [[Traverse]] for `F`.
   */
  @inline def apply[F[_]](implicit instance: Traverse[F]): Traverse[F] = instance

  @deprecated("Use cats.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllTraverseOps[F[_], A](target: F[A])(implicit tc: Traverse[F]): AllOps[F, A] {
      type TypeClassType = Traverse[F]
    } =
      new AllOps[F, A] {
        type TypeClassType = Traverse[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  trait Ops[F[_], A] extends Serializable {
    type TypeClassType <: Traverse[F]
    def self: F[A]
    val typeClassInstance: TypeClassType
    def traverse[G[_], B](f: A => G[B])(implicit ev$1: Applicative[G]): G[F[B]] =
      typeClassInstance.traverse[G, A, B](self)(f)
    def traverseTap[G[_], B](f: A => G[B])(implicit ev$1: Applicative[G]): G[F[A]] =
      typeClassInstance.traverseTap[G, A, B](self)(f)
    def flatTraverse[G[_], B](f: A => G[F[B]])(implicit G: Applicative[G], F: FlatMap[F]): G[F[B]] =
      typeClassInstance.flatTraverse[G, A, B](self)(f)(G, F)
    def sequence[G[_], B](implicit ev$1: A <:< G[B], ev$2: Applicative[G]): G[F[B]] =
      typeClassInstance.sequence[G, B](self.asInstanceOf[F[G[B]]])
    def flatSequence[G[_], B](implicit ev$1: A <:< G[F[B]], G: Applicative[G], F: FlatMap[F]): G[F[B]] =
      typeClassInstance.flatSequence[G, B](self.asInstanceOf[F[G[F[B]]]])(G, F)
    def mapAccumulate[S, B](init: S)(f: (S, A) => (S, B)): (S, F[B]) =
      typeClassInstance.mapAccumulate[S, A, B](init, self)(f)
    def mapWithIndex[B](f: (A, Int) => B): F[B] =
      typeClassInstance.mapWithIndex[A, B](self)(f)
    def traverseWithIndexM[G[_], B](f: (A, Int) => G[B])(implicit G: Monad[G]): G[F[B]] =
      typeClassInstance.traverseWithIndexM[G, A, B](self)(f)(G)
    def zipWithIndex: F[(A, Int)] =
      typeClassInstance.zipWithIndex[A](self)
    def zipWithLongIndex: F[(A, Long)] =
      typeClassInstance.zipWithLongIndex[A](self)
    def traverseWithLongIndexM[G[_], B](f: (A, Long) => G[B])(implicit G: Monad[G]): G[F[B]] =
      typeClassInstance.traverseWithLongIndexM[G, A, B](self)(f)
    def mapWithLongIndex[B](f: (A, Long) => B): F[B] =
      typeClassInstance.mapWithLongIndex[A, B](self)(f)
    def updated_[B >: A](idx: Long, b: B): Option[F[B]] =
      typeClassInstance.updated_(self, idx, b)
  }
  trait AllOps[F[_], A]
      extends Ops[F, A]
      with Functor.AllOps[F, A]
      with Foldable.AllOps[F, A]
      with UnorderedTraverse.AllOps[F, A] {
    type TypeClassType <: Traverse[F]
  }
  trait ToTraverseOps extends Serializable {
    implicit def toTraverseOps[F[_], A](target: F[A])(implicit tc: Traverse[F]): Ops[F, A] {
      type TypeClassType = Traverse[F]
    } =
      new Ops[F, A] {
        type TypeClassType = Traverse[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToTraverseOps

  private[cats] def traverseDirectly[G[_], A, B](
    fa: IterableOnce[A]
  )(f: A => G[B])(implicit G: StackSafeMonad[G]): G[Chain[B]] = {
    fa.iterator.foldLeft(G.pure(Chain.empty[B])) { case (accG, a) =>
      G.map2(accG, f(a)) { case (acc, x) =>
        acc :+ x
      }
    }
  }

  private[cats] def traverse_Directly[G[_], A, B](
    fa: IterableOnce[A]
  )(f: A => G[B])(implicit G: StackSafeMonad[G]): G[Unit] = {
    val iter = fa.iterator
    if (iter.hasNext) {
      val first = iter.next()
      G.void(iter.foldLeft(f(first)) { case (g, a) =>
        G.productR(g)(f(a))
      })
    } else G.unit
  }

}
