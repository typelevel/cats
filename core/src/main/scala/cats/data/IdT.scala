package cats
package data

/**
 * `IdT[F[_], A]` is the identity monad transformer.
 */
final case class IdT[F[_], A](value: F[A]) {

  def map[B](f: A => B)(implicit F: Functor[F]): IdT[F, B] =
    IdT(F.map(value)(f))

  def flatMap[B](f: A => IdT[F, B])(implicit F: FlatMap[F]): IdT[F, B] =
    IdT(F.flatMap(value)(f.andThen(_.value)))

  def flatMapF[B](f: A => F[B])(implicit F: FlatMap[F]): IdT[F, B] =
    IdT(F.flatMap(value)(f))

  def foldLeft[B](b: B)(f: (B, A) => B)(implicit F: Foldable[F]): B =
    F.foldLeft(value, b)(f)

  def foldRight[B](lb: Eval[B])(f: (A, Eval[B]) => Eval[B])(implicit F: Foldable[F]): Eval[B] =
    F.foldRight(value, lb)(f)

  def traverse[G[_], B](f: A => G[B])(implicit F: Traverse[F], G: Applicative[G]): G[IdT[F, B]] =
    G.map(F.traverse(value)(f))(IdT(_))

  def ap[B](f: IdT[F, A => B])(implicit F: Apply[F]): IdT[F, B] =
    IdT(F.ap(f.value)(value))

}

object IdT extends IdTInstances {

  def pure[F[_], A](a: A)(implicit F: Applicative[F]): IdT[F, A] =
    IdT(F.pure(a))
}

private[data] sealed trait IdTFunctor[F[_]] extends Functor[IdT[F, ?]] {
  implicit val F0: Functor[F]

  def map[A, B](fa: IdT[F, A])(f: A => B): IdT[F, B] =
    fa.map(f)
}

private[data] sealed trait IdTMonad[F[_]] extends Monad[IdT[F, ?]] {
  implicit val F0: Monad[F]

  def pure[A](a: A): IdT[F, A] =
    IdT.pure(a)

  def flatMap[A, B](fa: IdT[F, A])(f: A => IdT[F, B]): IdT[F, B] =
    fa.flatMap(f)

  def tailRecM[A, B](a: A)(f: A => IdT[F, Either[A, B]]): IdT[F, B] =
    IdT(F0.tailRecM(a)(f(_).value))
}

private[data] sealed trait IdTFoldable[F[_]] extends Foldable[IdT[F, ?]] {
  implicit val F0: Foldable[F]

  def foldLeft[A, B](fa: IdT[F, A], b: B)(f: (B, A) => B): B =
    fa.foldLeft(b)(f)

  def foldRight[A, B](fa: IdT[F, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    fa.foldRight(lb)(f)
}

private[data] sealed trait IdTTraverse[F[_]] extends Traverse[IdT[F, ?]] with IdTFoldable[F] {
  implicit val F0: Traverse[F]

  def traverse[G[_]: Applicative, A, B](fa: IdT[F, A])(f: A => G[B]): G[IdT[F, B]] =
    fa.traverse(f)
}

private[data] sealed abstract class IdTInstances1 {
  implicit def catsDataFunctorForIdT[F[_]](implicit F: Functor[F]): Functor[IdT[F, ?]] =
    new IdTFunctor[F] {
      implicit val F0: Functor[F] = F
    }
}

private[data] sealed abstract class IdTInstances0 extends IdTInstances1 {

  implicit def catsDataMonadForIdT[F[_]](implicit F: Monad[F]): Monad[IdT[F, ?]] =
    new IdTMonad[F] {
      implicit val F0: Monad[F] = F
    }

  implicit def catsDataRecursiveTailRecMForIdT[F[_]: RecursiveTailRecM]: RecursiveTailRecM[IdT[F, ?]] =
    RecursiveTailRecM.create[IdT[F, ?]]

  implicit def catsDataFoldableForIdT[F[_]](implicit F: Foldable[F]): Foldable[IdT[F, ?]] =
    new IdTFoldable[F] {
      implicit val F0: Foldable[F] = F
    }

  implicit def catsDataOrderForIdT[F[_], A](implicit F: Order[F[A]]): Order[IdT[F, A]] =
    F.on(_.value)
}

private[data] sealed abstract class IdTInstances extends IdTInstances0 {

  implicit def catsDataTraverseForIdT[F[_]](implicit F: Traverse[F]): Traverse[IdT[F, ?]] =
    new IdTTraverse[F] {
      implicit val F0: Traverse[F] = F
    }

  implicit def catsDataEqForIdT[F[_], A](implicit F: Eq[F[A]]): Eq[IdT[F, A]] =
    F.on(_.value)

  implicit def catsDataShowForIdT[F[_], A](implicit F: Show[F[A]]): Show[IdT[F, A]] =
    functor.Contravariant[Show].contramap(F)(_.value)
}
