package cats
package data

import cats.arrow.FunctionK
import cats.functor.Contravariant

/** `F` on the left and `G` on the right of [[cats.data.Xor]].
 *
 * @param run The underlying [[cats.data.Xor]].
 */
final case class Coproduct[F[_], G[_], A](run: Xor[F[A], G[A]]) {

  import Coproduct._

  def map[B](f: A => B)(implicit F: Functor[F], G: Functor[G]): Coproduct[F, G, B] =
    Coproduct(run.bimap(F.lift(f), G.lift(f)))

  def coflatMap[B](f: Coproduct[F, G, A] => B)(implicit F: CoflatMap[F], G: CoflatMap[G]): Coproduct[F, G, B] =
    Coproduct(
      run.bimap(a => F.coflatMap(a)(x => f(leftc(x))), a => G.coflatMap(a)(x => f(rightc(x))))
    )

  def coflatten(implicit F: CoflatMap[F], G: CoflatMap[G]): Coproduct[F, G, Coproduct[F, G, A]] =
    Coproduct(run.bimap(
      x => F.coflatMap(x)(a => leftc(a))
      , x => G.coflatMap(x)(a => rightc(a)))
    )

  def extract(implicit F: Comonad[F], G: Comonad[G]): A =
    run.fold(F.extract, G.extract)

  def contramap[B](f: B => A)(implicit F: Contravariant[F], G: Contravariant[G]): Coproduct[F, G, B] =
    Coproduct(run.bimap(F.contramap(_)(f), G.contramap(_)(f)))

  def foldRight[B](z: Eval[B])(f: (A, Eval[B]) => Eval[B])(implicit F: Foldable[F], G: Foldable[G]): Eval[B] =
    run.fold(a => F.foldRight(a, z)(f), a => G.foldRight(a, z)(f))

  def foldLeft[B](z: B)(f: (B, A) => B)(implicit F: Foldable[F], G: Foldable[G]): B =
    run.fold(a => F.foldLeft(a, z)(f), a => G.foldLeft(a, z)(f))

  def foldMap[B](f: A => B)(implicit F: Foldable[F], G: Foldable[G], M: Monoid[B]): B =
    run.fold(F.foldMap(_)(f), G.foldMap(_)(f))

  def traverse[X[_], B](g: A => X[B])(implicit F: Traverse[F], G: Traverse[G], A: Applicative[X]): X[Coproduct[F, G, B]] =
    run.fold(
      x => A.map(F.traverse(x)(g))(leftc(_))
      , x => A.map(G.traverse(x)(g))(rightc(_))
    )

  def isLeft: Boolean =
    run.isLeft

  def isRight: Boolean =
    run.isRight

  def swap: Coproduct[G, F, A] =
    Coproduct(run.swap)

  def toValidated: Validated[F[A], G[A]] =
    run.toValidated

  /**
   * Fold this coproduct into a new type constructor using two natural transformations.
   *
   * Example:
   * {{{
   * scala> import cats.arrow.FunctionK
   * scala> import cats.data.Coproduct
   * scala> val listToOption =
   *      |   new FunctionK[List, Option] {
   *      |     def apply[A](fa: List[A]): Option[A] = fa.headOption
   *      |   }
   * scala> val optionToOption = FunctionK.id[Option]
   * scala> val cp1: Coproduct[List, Option, Int] = Coproduct.leftc(List(1,2,3))
   * scala> val cp2: Coproduct[List, Option, Int] = Coproduct.rightc(Some(4))
   * scala> cp1.fold(listToOption, optionToOption)
   * res0: Option[Int] = Some(1)
   * scala> cp2.fold(listToOption, optionToOption)
   * res1: Option[Int] = Some(4)
   * }}}
   */
  def fold[H[_]](f: FunctionK[F, H], g: FunctionK[G, H]): H[A] =
    run.fold(f.apply, g.apply)
}

object Coproduct extends CoproductInstances {

  def leftc[F[_], G[_], A](x: F[A]): Coproduct[F, G, A] =
    Coproduct(Xor.Left(x))

  def rightc[F[_], G[_], A](x: G[A]): Coproduct[F, G, A] =
    Coproduct(Xor.Right(x))

  final class CoproductLeft[G[_]] private[Coproduct] {
    def apply[F[_], A](fa: F[A]): Coproduct[F, G, A] = Coproduct(Xor.Left(fa))
  }

  final class CoproductRight[F[_]] private[Coproduct] {
    def apply[G[_], A](ga: G[A]): Coproduct[F, G, A] = Coproduct(Xor.Right(ga))
  }

  def left[G[_]]: CoproductLeft[G] = new CoproductLeft[G]

  def right[F[_]]: CoproductRight[F] = new CoproductRight[F]
}

private[data] sealed abstract class CoproductInstances3 {

  implicit def catsDataEqForCoproduct[F[_], G[_], A](implicit E: Eq[Xor[F[A], G[A]]]): Eq[Coproduct[F, G, A]] =
    Eq.by(_.run)

  implicit def catsDataFunctorForCoproduct[F[_], G[_]](implicit F0: Functor[F], G0: Functor[G]): Functor[Coproduct[F, G, ?]] =
    new CoproductFunctor[F, G] {
      implicit def F: Functor[F] = F0

      implicit def G: Functor[G] = G0
    }

  implicit def catsDataFoldableForCoproduct[F[_], G[_]](implicit F0: Foldable[F], G0: Foldable[G]): Foldable[Coproduct[F, G, ?]] =
    new CoproductFoldable[F, G] {
      implicit def F: Foldable[F] = F0

      implicit def G: Foldable[G] = G0
    }
}

private[data] sealed abstract class CoproductInstances2 extends CoproductInstances3 {

  implicit def catsDataContravariantForCoproduct[F[_], G[_]](implicit F0: Contravariant[F], G0: Contravariant[G]): Contravariant[Coproduct[F, G, ?]] =
    new CoproductContravariant[F, G] {
      implicit def F: Contravariant[F] = F0

      implicit def G: Contravariant[G] = G0
    }
}

private[data] sealed abstract class CoproductInstances1 extends CoproductInstances2 {
  implicit def catsDataCoflatMapForCoproduct[F[_], G[_]](implicit F0: CoflatMap[F], G0: CoflatMap[G]): CoflatMap[Coproduct[F, G, ?]] =
    new CoproductCoflatMap[F, G] {
      implicit def F: CoflatMap[F] = F0

      implicit def G: CoflatMap[G] = G0
    }
}

private[data] sealed abstract class CoproductInstances0 extends CoproductInstances1 {
  implicit def catsDataTraverseForCoproduct[F[_], G[_]](implicit F0: Traverse[F], G0: Traverse[G]): Traverse[Coproduct[F, G, ?]] =
    new CoproductTraverse[F, G] {
      implicit def F: Traverse[F] = F0

      implicit def G: Traverse[G] = G0
    }
}

sealed abstract class CoproductInstances extends CoproductInstances0 {

  implicit def catsDataComonadForCoproduct[F[_], G[_]](implicit F0: Comonad[F], G0: Comonad[G]): Comonad[Coproduct[F, G, ?]] =
    new CoproductComonad[F, G] {
      implicit def F: Comonad[F] = F0

      implicit def G: Comonad[G] = G0
    }
}

private[data] trait CoproductFunctor[F[_], G[_]] extends Functor[Coproduct[F, G, ?]] {
  implicit def F: Functor[F]

  implicit def G: Functor[G]

  def map[A, B](a: Coproduct[F, G, A])(f: A => B): Coproduct[F, G, B] =
    a map f
}

private[data] trait CoproductContravariant[F[_], G[_]] extends Contravariant[Coproduct[F, G, ?]] {
  implicit def F: Contravariant[F]

  implicit def G: Contravariant[G]

  def contramap[A, B](a: Coproduct[F, G, A])(f: B => A): Coproduct[F, G, B] =
    a contramap f
}

private[data] trait CoproductFoldable[F[_], G[_]] extends Foldable[Coproduct[F, G, ?]] {
  implicit def F: Foldable[F]

  implicit def G: Foldable[G]

  def foldRight[A, B](fa: Coproduct[F, G, A], z: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    fa.foldRight(z)(f)

  def foldLeft[A, B](fa: Coproduct[F, G, A], z: B)(f: (B, A) => B): B =
    fa.foldLeft(z)(f)

  override def foldMap[A, B](fa: Coproduct[F, G, A])(f: A => B)(implicit M: Monoid[B]): B =
    fa foldMap f
}

private[data] trait CoproductTraverse[F[_], G[_]] extends CoproductFoldable[F, G] with Traverse[Coproduct[F, G, ?]] {
  implicit def F: Traverse[F]

  implicit def G: Traverse[G]

  override def map[A, B](a: Coproduct[F, G, A])(f: A => B): Coproduct[F, G, B] =
    a map f

  override def traverse[X[_] : Applicative, A, B](fa: Coproduct[F, G, A])(f: A => X[B]): X[Coproduct[F, G, B]] =
    fa traverse f
}

private[data] trait CoproductCoflatMap[F[_], G[_]] extends CoflatMap[Coproduct[F, G, ?]] {
  implicit def F: CoflatMap[F]

  implicit def G: CoflatMap[G]

  def map[A, B](a: Coproduct[F, G, A])(f: A => B): Coproduct[F, G, B] =
    a map f

  def coflatMap[A, B](a: Coproduct[F, G, A])(f: Coproduct[F, G, A] => B): Coproduct[F, G, B] =
    a coflatMap f

  override def coflatten[A](fa: Coproduct[F, G, A]): Coproduct[F, G, Coproduct[F, G, A]] =
    fa.coflatten
}

private[data] trait CoproductComonad[F[_], G[_]] extends Comonad[Coproduct[F, G, ?]] with CoproductCoflatMap[F, G] {
  implicit def F: Comonad[F]

  implicit def G: Comonad[G]

  def extract[A](p: Coproduct[F, G, A]): A =
    p.extract
}

