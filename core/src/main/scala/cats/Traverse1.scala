package cats

import simulacrum.typeclass

/**
  * Traverse1, also known as Traversable1.
  *
  * `Traverse1` is like a non-empty `Traverse`. In addition to the traverse and sequence
  * methods it provides traverse1 and sequence1 methods which require an `Apply` instance instead of `Applicative`.
  */
@typeclass trait Traverse1[F[_]] extends Traverse[F] with Reducible[F] {

  def traverse1[G[_]: Apply, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

  def sequence1[G[_]: Apply, A](fga: F[G[A]]): G[F[A]] =
    traverse1(fga)(identity)

  def flatTraverse1[G[_], A, B](fa: F[A])(f: A => G[F[B]])(implicit G: Apply[G], F: FlatMap[F]): G[F[B]] =
    G.map(traverse1(fa)(f))(F.flatten)

  def flatSequence1[G[_], A](fgfa: F[G[F[A]]])(implicit G: Apply[G], F: FlatMap[F]): G[F[A]] =
    G.map(traverse1(fgfa)(identity))(F.flatten)

  override def traverse[G[_] : Applicative, A, B](fa: F[A])(f: (A) => G[B]): G[F[B]] =
    traverse1(fa)(f)



}
