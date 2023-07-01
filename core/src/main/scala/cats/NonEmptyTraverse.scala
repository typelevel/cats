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
 * NonEmptyTraverse, also known as Traversable1.
 *
 * `NonEmptyTraverse` is like a non-empty `Traverse`. In addition to the traverse and sequence
 * methods it provides nonEmptyTraverse and nonEmptySequence methods which require an `Apply` instance instead of `Applicative`.
 */
trait NonEmptyTraverse[F[_]] extends Traverse[F] with Reducible[F] { self =>

  /**
   * Given a function which returns a G effect, thread this effect
   * through the running of this function on all the values in F,
   * returning an F[B] in a G context.
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> import cats.data.NonEmptyList
   * scala> def countWords(words: List[String]): Map[String, Int] = words.groupBy(identity).map { case (k, v) => (k, v.length) }
   * scala> val expectedResult = Map("do" -> NonEmptyList.of(1, 2), "you" -> NonEmptyList.of(1, 1))
   * scala> val x = List("How", "do", "you", "fly")
   * scala> val y = List("What", "do", "you", "do")
   * scala> val result = NonEmptyList.of(x, y).nonEmptyTraverse(countWords)
   * scala> result === expectedResult
   * res0: Boolean = true
   * }}}
   */
  def nonEmptyTraverse[G[_]: Apply, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

  /**
   * Thread all the G effects through the F structure to invert the
   * structure from F[G[A]] to G[F[A]].
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> import cats.data.NonEmptyList
   * scala> val x = NonEmptyList.of(Map("do" -> 1, "you" -> 1), Map("do" -> 2, "you" -> 1))
   * scala> val y = NonEmptyList.of(Map("How" -> 3, "do" -> 1, "you" -> 1), Map[String,Int]())
   * scala> x.nonEmptySequence
   * res0: Map[String,NonEmptyList[Int]] = Map(do -> NonEmptyList(1, 2), you -> NonEmptyList(1, 1))
   * scala> y.nonEmptySequence
   * res1: Map[String,NonEmptyList[Int]] = Map()
   * }}}
   */
  def nonEmptySequence[G[_]: Apply, A](fga: F[G[A]]): G[F[A]] =
    nonEmptyTraverse(fga)(identity)

  /**
   * A nonEmptyTraverse followed by flattening the inner result.
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> import cats.data.NonEmptyList
   * scala> val x = NonEmptyList.of(List("How", "do", "you", "fly"), List("What", "do", "you", "do"))
   * scala> x.nonEmptyFlatTraverse(_.groupByNel(identity) : Map[String, NonEmptyList[String]])
   * res0: Map[String,cats.data.NonEmptyList[String]] = Map(do -> NonEmptyList(do, do, do), you -> NonEmptyList(you, you))
   * }}}
   */
  def nonEmptyFlatTraverse[G[_], A, B](fa: F[A])(f: A => G[F[B]])(implicit G: Apply[G], F: FlatMap[F]): G[F[B]] =
    G.map(nonEmptyTraverse(fa)(f))(F.flatten)

  /**
   * Thread all the G effects through the F structure and flatten to invert the
   * structure from F[G[F[A]]] to G[F[A]].
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> import cats.data.NonEmptyList
   * scala> val x = NonEmptyList.of(Map(0 ->NonEmptyList.of(1, 2)), Map(0 -> NonEmptyList.of(3)))
   * scala> val y: NonEmptyList[Map[Int, NonEmptyList[Int]]] = NonEmptyList.of(Map(), Map(1 -> NonEmptyList.of(3)))
   * scala> x.nonEmptyFlatSequence
   * res0: Map[Int,cats.data.NonEmptyList[Int]] = Map(0 -> NonEmptyList(1, 2, 3))
   * scala> y.nonEmptyFlatSequence
   * res1: Map[Int,cats.data.NonEmptyList[Int]] = Map()
   * }}}
   */
  def nonEmptyFlatSequence[G[_], A](fgfa: F[G[F[A]]])(implicit G: Apply[G], F: FlatMap[F]): G[F[A]] =
    G.map(nonEmptyTraverse(fgfa)(identity))(F.flatten)

  override def traverse[G[_]: Applicative, A, B](fa: F[A])(f: (A) => G[B]): G[F[B]] =
    nonEmptyTraverse(fa)(f)

  def compose[G[_]: NonEmptyTraverse]: NonEmptyTraverse[λ[α => F[G[α]]]] =
    new ComposedNonEmptyTraverse[F, G] {
      val F = self
      val G = NonEmptyTraverse[G]
    }

}

object NonEmptyTraverse {

  /**
   * Summon an instance of [[NonEmptyTraverse]] for `F`.
   */
  @inline def apply[F[_]](implicit instance: NonEmptyTraverse[F]): NonEmptyTraverse[F] = instance

  @deprecated("Use cats.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllNonEmptyTraverseOps[F[_], A](target: F[A])(implicit tc: NonEmptyTraverse[F]): AllOps[F, A] {
      type TypeClassType = NonEmptyTraverse[F]
    } =
      new AllOps[F, A] {
        type TypeClassType = NonEmptyTraverse[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  trait Ops[F[_], A] extends Serializable {
    type TypeClassType <: NonEmptyTraverse[F]
    def self: F[A]
    val typeClassInstance: TypeClassType
    def nonEmptyTraverse[G[_], B](f: A => G[B])(implicit ev$1: Apply[G]): G[F[B]] =
      typeClassInstance.nonEmptyTraverse[G, A, B](self)(f)
    def nonEmptySequence[G[_], B](implicit ev$1: A <:< G[B], ev$2: Apply[G]): G[F[B]] =
      typeClassInstance.nonEmptySequence[G, B](self.asInstanceOf[F[G[B]]])
    def nonEmptyFlatTraverse[G[_], B](f: A => G[F[B]])(implicit G: Apply[G], F: FlatMap[F]): G[F[B]] =
      typeClassInstance.nonEmptyFlatTraverse[G, A, B](self)(f)(G, F)
    def nonEmptyFlatSequence[G[_], B](implicit ev$1: A <:< G[F[B]], G: Apply[G], F: FlatMap[F]): G[F[B]] =
      typeClassInstance.nonEmptyFlatSequence[G, B](self.asInstanceOf[F[G[F[B]]]])(G, F)
  }
  trait AllOps[F[_], A] extends Ops[F, A] with Traverse.AllOps[F, A] with Reducible.AllOps[F, A] {
    type TypeClassType <: NonEmptyTraverse[F]
  }
  trait ToNonEmptyTraverseOps extends Serializable {
    implicit def toNonEmptyTraverseOps[F[_], A](target: F[A])(implicit tc: NonEmptyTraverse[F]): Ops[F, A] {
      type TypeClassType = NonEmptyTraverse[F]
    } =
      new Ops[F, A] {
        type TypeClassType = NonEmptyTraverse[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToNonEmptyTraverseOps

}
