package cats
package data

import cats.Contravariant
import cats.kernel.{CommutativeMonoid, CommutativeSemigroup}

/**
 * [[Const]] is a phantom type, it does not contain a value of its second type parameter `B`
 * [[Const]] can be seen as a type level version of `Function.const[A, B]: A => B => A`
 */
final case class Const[A, B](getConst: A) {
  /**
   * changes the type of the second type parameter
   */
  def retag[C]: Const[A, C] =
    this.asInstanceOf[Const[A, C]]

  def combine(that: Const[A, B])(implicit A: Semigroup[A]): Const[A, B] =
    Const(A.combine(getConst, that.getConst))

  def traverse[F[_], C](f: B => F[C])(implicit F: Applicative[F]): F[Const[A, C]] =
    F.pure(retag[C])

  def ===(that: Const[A, B])(implicit A: Eq[A]): Boolean =
    A.eqv(getConst, that.getConst)

  def partialCompare(that: Const[A, B])(implicit A: PartialOrder[A]): Double =
    A.partialCompare(getConst, that.getConst)

  def compare(that: Const[A, B])(implicit A: Order[A]): Int =
    A.compare(getConst, that.getConst)

  def show(implicit A: Show[A]): String =
    s"Const(${A.show(getConst)})"
}

object Const extends ConstInstances {
  def empty[A, B](implicit A: Monoid[A]): Const[A, B] =
    Const(A.empty)

  /**
   * Uses the [[http://typelevel.org/cats/guidelines.html#partially-applied-type-params Partially Applied Type Params technique]] for ergonomics.
   */
  private[data] final class OfPartiallyApplied[B](val dummy: Boolean = true ) extends AnyVal {
    def apply[A](a: A): Const[A, B] = Const(a)
  }

  /**
   * Convenient syntax for creating a Const[A, B] from an `A`
   * {{{
   * scala> import cats.data._
   * scala> Const.of[Int]("a")
   * res0: Const[String, Int] = Const(a)
   * }}}
   */
  def of[B]: OfPartiallyApplied[B] = new OfPartiallyApplied
}

private[data] sealed abstract class ConstInstances extends ConstInstances0 {
  implicit def catsDataOrderForConst[A: Order, B]: Order[Const[A, B]] = new Order[Const[A, B]] {
    def compare(x: Const[A, B], y: Const[A, B]): Int =
      x compare y
  }

  implicit def catsDataShowForConst[A: Show, B]: Show[Const[A, B]] = new Show[Const[A, B]] {
    def show(f: Const[A, B]): String = f.show
  }

  implicit def catsDataContravariantMonoidalForConst[D: Monoid]: ContravariantMonoidal[Const[D, ?]] = new ContravariantMonoidal[Const[D, ?]] {
    override def unit = Const.empty[D, Unit]
    override def contramap[A, B](fa: Const[D, A])(f: B => A): Const[D, B] =
      fa.retag[B]
    override def product[A, B](fa: Const[D, A], fb: Const[D, B]): Const[D, (A, B)] =
      fa.retag[(A, B)] combine fb.retag[(A, B)]
  }

  implicit def catsDataTraverseForConst[C]: Traverse[Const[C, ?]] = new Traverse[Const[C, ?]] {
    def foldLeft[A, B](fa: Const[C, A], b: B)(f: (B, A) => B): B = b

    def foldRight[A, B](fa: Const[C, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = lb

    override def size[A](fa: Const[C, A]): Long = 0L

    override def get[A](fa: Const[C, A])(idx: Long): Option[A] = None

    def traverse[G[_]: Applicative, A, B](fa: Const[C, A])(f: A => G[B]): G[Const[C, B]] =
      fa.traverse(f)
  }

  implicit def catsDataMonoidForConst[A: Monoid, B]: Monoid[Const[A, B]] = new Monoid[Const[A, B]]{
    def empty: Const[A, B] =
      Const.empty

    def combine(x: Const[A, B], y: Const[A, B]): Const[A, B] =
      x combine y
  }

  implicit val catsDataBifoldableForConst: Bifoldable[Const] =
    new Bifoldable[Const] {
      def bifoldLeft[A, B, C](fab: Const[A, B], c: C)(f: (C, A) => C, g: (C, B) => C): C =
        f(c, fab.getConst)

      def bifoldRight[A, B, C](fab: Const[A, B], c: Eval[C])(f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C]): Eval[C] =
        f(fab.getConst, c)
    }
}

private[data] sealed abstract class ConstInstances0 extends ConstInstances1 {

  implicit def catsDataCommutativeApplicativeForConst[C](implicit C: CommutativeMonoid[C]): CommutativeApplicative[Const[C, ?]] =
    new ConstApplicative[C] with CommutativeApplicative[Const[C, ?]] { val C0: CommutativeMonoid[C] = C }
}

private[data] sealed abstract class ConstInstances1 extends ConstInstances2 {

  implicit def catsDataCommutativeApplyForConst[C](implicit C: CommutativeSemigroup[C]): CommutativeApply[Const[C, ?]] =
    new ConstApply[C] with CommutativeApply[Const[C, ?]] { val C0: CommutativeSemigroup[C] = C }
}

private[data] sealed abstract class ConstInstances2 extends ConstInstances3 {

  implicit def catsDataSemigroupForConst[A: Semigroup, B]: Semigroup[Const[A, B]] = new Semigroup[Const[A, B]] {
    def combine(x: Const[A, B], y: Const[A, B]): Const[A, B] = x combine y
  }

  implicit def catsDataContravariantForConst[C]: Contravariant[Const[C, ?]] = new Contravariant[Const[C, ?]] {
    override def contramap[A, B](fa: Const[C, A])(f: (B) => A): Const[C, B] =
      fa.retag[B]
  }

  implicit def catsDataPartialOrderForConst[A: PartialOrder, B]: PartialOrder[Const[A, B]] = new PartialOrder[Const[A, B]]{
    def partialCompare(x: Const[A, B], y: Const[A, B]): Double =
      x partialCompare y
  }

  implicit def catsDataApplicativeForConst[C](implicit C: Monoid[C]): Applicative[Const[C, ?]] =
    new ConstApplicative[C] { val C0: Monoid[C] = C }
}

private[data] sealed abstract class ConstInstances3 extends ConstInstances4 {

  implicit def catsDataEqForConst[A: Eq, B]: Eq[Const[A, B]] = new Eq[Const[A, B]] {
    def eqv(x: Const[A, B], y: Const[A, B]): Boolean =
      x === y
  }

  implicit def catsDataApplyForConst[C](implicit C: Semigroup[C]): Apply[Const[C, ?]] =
    new ConstApply[C] { val C0: Semigroup[C] = C }
}

private[data] sealed abstract class ConstInstances4 {

  implicit def catsDataFunctorForConst[C]: Functor[Const[C, ?]] =
    new ConstFunctor[C]{}
}

private[data] sealed trait ConstFunctor[C] extends Functor[Const[C, ?]] {
  def map[A, B](fa: Const[C, A])(f: A => B): Const[C, B] =
    fa.retag[B]
}

private[data] sealed trait ConstApply[C] extends ConstFunctor[C] with Apply[Const[C, ?]] {

  implicit def C0: Semigroup[C]

  def ap[A, B](f: Const[C, A => B])(fa: Const[C, A]): Const[C, B] =
    f.retag[B] combine fa.retag[B]

  override def product[A, B](fa: Const[C, A], fb: Const[C, B]): Const[C, (A, B)] =
    fa.retag[(A, B)] combine fb.retag[(A, B)]
}

private[data] sealed trait ConstApplicative[C] extends ConstApply[C] with Applicative[Const[C, ?]] {

  implicit def C0: Monoid[C]

  def pure[A](x: A): Const[C, A] =
    Const.empty
}
