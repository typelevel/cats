package cats

import cats.data.NonEmptyList
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

  def flatSequence[G[_], A](fgfa: F[G[F[A]]])(implicit G: Apply[G], F: FlatMap[F]): G[F[A]] =
    G.map(traverse1(fgfa)(identity))(F.flatten)

  override def traverse[G[_] : Applicative, A, B](fa: F[A])(f: (A) => G[B]): G[F[B]] =
    traverse1(fa)(f)

  override def sequence[G[_] : Applicative, A](fga: F[G[A]]): G[F[A]] =
    traverse1(fga)(identity)

  override def reduceMap[A, B](fa: F[A])(f: (A) => B)(implicit B: Semigroup[B]): B =
    reduceLeft(traverse1[Id, A, B](fa)(f))(B.combine)

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    traverse1[Id, A, B](fa)(f)

}

/**
  * This class defines a `Traverse1[F]` in terms of a `Traverse[G]`
  * together with a `split` method, `F[A]` => `(A, G[A])`.
  *
  * This class can be used on any type where the first value (`A`) and
  * the "rest" of the values (`G[A]`) can be easily found.
  */
abstract class NonEmptyTraverse1[F[_], G[_]](implicit G: Traverse[G]) extends Traverse1[F] {
  def split[A](fa: F[A]): (A, G[A])

  def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B = {
    val (a, ga) = split(fa)
    G.foldLeft(ga, f(b, a))(f)
  }

  def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    Always(split(fa)).flatMap { case (a, ga) =>
      f(a, G.foldRight(ga, lb)(f))
    }

  def reduceLeftTo[A, B](fa: F[A])(f: A => B)(g: (B, A) => B): B = {
    val (a, ga) = split(fa)
    G.foldLeft(ga, f(a))((b, a) => g(b, a))
  }

  def reduceRightTo[A, B](fa: F[A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B] =
    Always(split(fa)).flatMap { case (a, ga) =>
      G.reduceRightToOption(ga)(f)(g).flatMap {
        case Some(b) => g(a, Now(b))
        case None => Later(f(a))
      }
    }

  override def size[A](fa: F[A]): Long = {
    val (_, tail) = split(fa)
    1 + G.size(tail)
  }

  override def fold[A](fa: F[A])(implicit A: Monoid[A]): A = {
    val (a, ga) = split(fa)
    A.combine(a, G.fold(ga))
  }

  override def foldM[H[_], A, B](fa: F[A], z: B)(f: (B, A) => H[B])(implicit H: Monad[H]): H[B] = {
    val (a, ga) = split(fa)
    H.flatMap(f(z, a))(G.foldM(ga, _)(f))
  }

  override def find[A](fa: F[A])(f: A => Boolean): Option[A] = {
    val (a, ga) = split(fa)
    if (f(a)) Some(a) else G.find(ga)(f)
  }

  override def exists[A](fa: F[A])(p: A => Boolean): Boolean = {
    val (a, ga) = split(fa)
    p(a) || G.exists(ga)(p)
  }

  override def forall[A](fa: F[A])(p: A => Boolean): Boolean = {
    val (a, ga) = split(fa)
    p(a) && G.forall(ga)(p)
  }

  override def toList[A](fa: F[A]): List[A] = {
    val (a, ga) = split(fa)
    a :: G.toList(ga)
  }

  override def toNonEmptyList[A](fa: F[A]): NonEmptyList[A] = {
    val (a, ga) = split(fa)
    NonEmptyList(a, G.toList(ga))
  }

  override def filter_[A](fa: F[A])(p: A => Boolean): List[A] = {
    val (a, ga) = split(fa)
    val filteredTail = G.filter_(ga)(p)
    if (p(a)) a :: filteredTail else filteredTail
  }

  override def takeWhile_[A](fa: F[A])(p: A => Boolean): List[A] = {
    val (a, ga) = split(fa)
    if (p(a)) a :: G.takeWhile_(ga)(p) else Nil
  }

  override def dropWhile_[A](fa: F[A])(p: A => Boolean): List[A] = {
    val (a, ga) = split(fa)
    if (p(a)) G.dropWhile_(ga)(p) else a :: G.toList(ga)
  }



}
