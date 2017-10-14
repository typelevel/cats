package cats

import simulacrum.typeclass

/**
  * NonEmptyTraverse, also known as Traversable1.
  *
  * `NonEmptyTraverse` is like a non-empty `Traverse`. In addition to the traverse and sequence
  * methods it provides nonEmptyTraverse and nonEmptySequence methods which require an `Apply` instance instead of `Applicative`.
  */
@typeclass trait NonEmptyTraverse[F[_]] extends Traverse[F] with Reducible[F] { self =>

  /**
    * Given a function which returns a G effect, thread this effect
    * through the running of this function on all the values in F,
    * returning an F[B] in a G context.
    *
    * Example:
    * {{{
    * scala> import cats.implicits._
    * scala> import cats.data.NonEmptyList
    * scala> def countWords(words: List[String]): Map[String, Int] = words.groupBy(identity).mapValues(_.length)
    * scala> NonEmptyList.of(List("How", "do", "you", "fly"), List("What", "do", "you", "do")).nonEmptyTraverse(countWords)
    * res0: Map[String,cats.data.NonEmptyList[Int]] = Map(do -> NonEmptyList(1, 2), you -> NonEmptyList(1, 1))
    * }}}
    */
  def nonEmptyTraverse[G[_]: Apply, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

  /**
    * Thread all the G effects through the F structure to invert the
    * structure from F[G[A]] to G[F[A]].
    *
    * Example:
    * {{{
    * scala> import cats.implicits._
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
    * scala> import cats.implicits._
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
    * scala> import cats.implicits._
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

  override def traverse[G[_] : Applicative, A, B](fa: F[A])(f: (A) => G[B]): G[F[B]] =
    nonEmptyTraverse(fa)(f)

  def compose[G[_]: NonEmptyTraverse]: NonEmptyTraverse[λ[α => F[G[α]]]] =
    new ComposedNonEmptyTraverse[F, G] {
      val F = self
      val G = NonEmptyTraverse[G]
    }


}
