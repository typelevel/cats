package cats

import simulacrum.typeclass

/**
  * Traverse1, also known as Traversable1.
  *
  * `Traverse1` is like a non-empty `Traverse`. In addition to the traverse and sequence
  * methods it provides traverse1 and sequence1 methods which require an `Apply` instance instead of `Applicative`.
  */
@typeclass trait Traverse1[F[_]] extends Traverse[F] with Reducible[F] {

  /**
    * Given a function which returns a G effect, thread this effect
    * through the running of this function on all the values in F,
    * returning an F[B] in a G context.
    *
    * Example:
    * {{{
    * scala> import cats.implicits._
    * scala> def countWords(words: List[String]): Map[String, Int] = words.groupBy(identity).mapValues(_.length)
    * scala> NonEmptyList.of(List("How", "do", "you", "fly"), List("What", "do", "you", "do")).traverse1(countWords)
    * res0:Map[String,cats.data.NonEmptyList[Int]] = Map(do -> NonEmptyList(1, 2), you -> NonEmptyList(1, 1))
    * }}}
    */
  def traverse1[G[_]: Apply, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

  /**
    * Thread all the G effects through the F structure to invert the
    * structure from F[G[A]] to G[F[A]].
    *
    * Example:
    * {{{
    * scala> import cats.implicits._
    * scala> val x = NonEmptyList(Map(do -> 1, How -> 1, you -> 1, fly -> 1), Map(do -> 2, you -> 1, What -> 1))
    * scala> x.sequence1
    * res0: Map[String,cats.data.NonEmptyList[Int]] = Map(do -> NonEmptyList(1, 2), you -> NonEmptyList(1, 1))
    * }}}
    */
  def sequence1[G[_]: Apply, A](fga: F[G[A]]): G[F[A]] =
    traverse1(fga)(identity)

  def flatTraverse1[G[_], A, B](fa: F[A])(f: A => G[F[B]])(implicit G: Apply[G], F: FlatMap[F]): G[F[B]] =
    G.map(traverse1(fa)(f))(F.flatten)

  def flatSequence1[G[_], A](fgfa: F[G[F[A]]])(implicit G: Apply[G], F: FlatMap[F]): G[F[A]] =
    G.map(traverse1(fgfa)(identity))(F.flatten)

  override def traverse[G[_] : Applicative, A, B](fa: F[A])(f: (A) => G[B]): G[F[B]] =
    traverse1(fa)(f)



}
