package cats.kernel

/**
 * Lifting of the [[Eq]] class to unary type constructors.
 *
 * @see [[https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-Functor-Classes.html#t:Eq1]]
 */
trait Eq1[F[_]] extends Any with Serializable {

  /**
   * Lift equality tests through the type constructor.
   */
  def liftEq[A, B](compare: (A, B) => Boolean, x: F[A], y: F[B]): Boolean

  // derived //

  /**
   * Returns `true` if `x` and `y` are equivalent, `false` otherwise.
   */
  def eqv1[A](x: F[A], y: F[A])(implicit A: Eq[A]): Boolean =
    liftEq[A, A](A.eqv, x, y)

  /**
   * Returns `false` if `x` and `y` are equivalent, `true` otherwise.
   */
  def neqv1[A: Eq](x: F[A], y: F[A]): Boolean =
    !eqv1[A](x, y)
}

object Eq1 extends Eq1Instances0 {
  @inline def apply[F[_]](implicit ev: Eq1[F]): Eq1[F] = ev
}

private[kernel] trait Eq1Instances0 extends Eq1LowPriorityInstances0 {

  /** @see [[Order1#catsKernelOrder1InstanceForId]] */
  implicit def catsKernelEq1InstanceForId: Eq1[({ type Id[α] = α })#Id] =
    Order1.catsKernelOrder1InstanceForId
}

private[kernel] trait Eq1LowPriorityInstances0 {

  /**
   * Derive an [[Eq1]] instance from an [[Eq2]] instance if we have an [[Eq]]
   * instance for the second type paremeter.
   */
  implicit def eq2ToEq1L[F[_, _], A](implicit F: Eq2[F], A: Eq[A]): Eq1[({ type L[α] = F[α, A] })#L] =
    new Eq1[({ type L[α] = F[α, A] })#L] {
      override def liftEq[B, C](compare: (B, C) => Boolean, x: F[B, A], y: F[C, A]): Boolean =
        F.liftEq2[B, C, A, A](compare, A.eqv, x, y)
    }

  /**
   * Derive an [[Eq1]] instance from an [[Eq2]] instance if we have an [[Eq]]
   * instance for the first type paremeter.
   */
  implicit def eq2ToEq1R[F[_, _], A](implicit F: Eq2[F], A: Eq[A]): Eq1[({ type L[α] = F[A, α] })#L] =
    new Eq1[({ type L[α] = F[A, α] })#L] {
      override def liftEq[B, C](compare: (B, C) => Boolean, x: F[A, B], y: F[A, C]): Boolean =
        F.liftEq2[A, A, B, C](A.eqv, compare, x, y)
    }
}
