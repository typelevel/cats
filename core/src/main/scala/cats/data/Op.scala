package cats
package data

import cats.arrow._

/**
 * The dual category of some other category, `Arr`.
 */
final case class Op[Arr[_, _], A, B](run: Arr[B, A]) {
  def compose[Z](op: Op[Arr, Z, A])(implicit Arr: Compose[Arr]): Op[Arr, Z, B] =
    Op(Arr.compose(op.run, run))

  def eqv(op: Op[Arr, A, B])(implicit Arr: Eq[Arr[B, A]]): Boolean =
    Arr.eqv(run, op.run)
}

object Op extends OpInstances

sealed abstract private[data] class OpInstances extends OpInstances0 {
  implicit def catsDataCategoryForOp[Arr[_, _]](implicit ArrC: Category[Arr]): Category[Op[Arr, *, *]] =
    new OpCategory[Arr] { def Arr: Category[Arr] = ArrC }

  implicit def catsDataDecidableForOp[Arr[_, _], R](implicit
    ArrC: ArrowChoice[Arr],
    monn: Monoid[R]
  ): Decidable[Op[Arr, R, ?]] =
    new OpDecidable[Arr, R] {
      def Arr: ArrowChoice[Arr] = ArrC
      def M: Monoid[R] = monn
    }

  implicit def catsDataEqForOp[Arr[_, _], A, B](implicit ArrEq: Eq[Arr[B, A]]): Eq[Op[Arr, A, B]] =
    new OpEq[Arr, A, B] { def Arr: Eq[Arr[B, A]] = ArrEq }

  @deprecated("Use catsDataEqForOp", "2.0.0-RC2")
  def catsKernelEqForOp[Arr[_, _], A, B](implicit ArrEq: Eq[Arr[B, A]]): Eq[Op[Arr, A, B]] =
    catsDataEqForOp[Arr, A, B]
}

sealed abstract private[data] class OpInstances0 {
  implicit def catsDataComposeForOp[Arr[_, _]](implicit ArrC: Compose[Arr]): Compose[Op[Arr, *, *]] =
    new OpCompose[Arr] { def Arr: Compose[Arr] = ArrC }

  implicit def catsDataContravariantMonoidalForOp[Arr[_, _], R](implicit
    ArrC: Arrow[Arr],
    M0: Monoid[R]
  ): ContravariantMonoidal[Op[Arr, R, ?]] =
    new OpContravariantMonoidal[Arr, R] { def Arr = ArrC; def M = M0 }
}

sealed abstract private[data] class OpInstances1 {
  implicit def catsDataContravariantForOp[Arr[_, _], R](implicit ArrC: Arrow[Arr]): Contravariant[Op[Arr, R, ?]] =
    new OpContravariant[Arr, R] { def Arr = ArrC }
}

private[data] trait OpCategory[Arr[_, _]] extends Category[Op[Arr, *, *]] with OpCompose[Arr] {
  implicit def Arr: Category[Arr]

  override def id[A]: Op[Arr, A, A] = Op(Arr.id)
}

private[data] trait OpCompose[Arr[_, _]] extends Compose[Op[Arr, *, *]] {
  implicit def Arr: Compose[Arr]

  def compose[A, B, C](f: Op[Arr, B, C], g: Op[Arr, A, B]): Op[Arr, A, C] =
    f.compose(g)
}

private[data] trait OpDecidable[Arr[_, _], R] extends Decidable[Op[Arr, R, ?]] with OpContravariantMonoidal[Arr, R] {
  implicit def Arr: ArrowChoice[Arr]
  implicit def M: Monoid[R]

  def sum[A, B](fa: Op[Arr, R, A], fb: Op[Arr, R, B]): Op[Arr, R, Either[A, B]] =
    Op(Arr.choice(fa.run, fb.run))

  override def zero[A]: Op[Arr, R, INothing] =
    Op(Arr.lift(INothing.absurd))
}

private[data] trait OpContravariantMonoidal[Arr[_, _], R]
    extends OpContravariant[Arr, R]
    with ContravariantMonoidal[Op[Arr, R, ?]] {
  implicit def Arr: Arrow[Arr]
  implicit def M: Monoid[R]

  def unit: Op[Arr, R, Unit] =
    Op(Arr.lift(Function.const(M.empty)))

  def product[A, B](fa: Op[Arr, R, A], fb: Op[Arr, R, B]): Op[Arr, R, (A, B)] =
    Op(Arr.compose(Arr.lift((M.combine _).tupled), Arr.split(fa.run, fb.run)))
}

private[data] trait OpContravariant[Arr[_, _], R] extends Contravariant[Op[Arr, R, ?]] {
  implicit def Arr: Arrow[Arr]

  def contramap[A, B](fa: Op[Arr, R, A])(f: B => A): Op[Arr, R, B] =
    Op(Arr.compose(fa.run, Arr.lift(f)))
}

private[data] trait OpEq[Arr[_, _], A, B] extends Eq[Op[Arr, A, B]] {
  implicit def Arr: Eq[Arr[B, A]]

  def eqv(f: Op[Arr, A, B], g: Op[Arr, A, B]): Boolean =
    f.eqv(g)
}
