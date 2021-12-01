package cats.kernel

/**
 * Lifting of the [[Hash]] class to unary type constructors.
 */
trait Hash1[F[_]] extends Any with Eq1[F] {

  /**
   * Lift hasing through the type constructor.
   */
  def liftHash[A](hash: A => Int, x: F[A]): Int

  // derived

  // other

  def hash1[A](x: F[A])(implicit A: Hash[A]): Int =
    liftHash(A.hash, x)
}

object Hash1 extends Hash1Instances0 {
  @inline def apply[F[_]](implicit ev: Hash1[F]): Hash1[F] = ev
}

private[kernel] trait Hash1Instances0 extends Hash1LowPriorityInstances0 {

  /** @see [[Order1#catsKernelOrder1InstanceForId]] */
  implicit val catsKernelHash1InstanceForId: Hash1[({ type Id[α] = α })#Id] =
    new Hash1[({ type Id[α] = α })#Id] {
      override def liftHash[A](hash: A => Int, x: A): Int =
        hash(x)

      override def liftEq[A, B](compare: (A, B) => Boolean, x: A, y: B): Boolean =
        compare(x, y)
    }
}

private[kernel] trait Hash1LowPriorityInstances0 {

  /**
   * Derive a [[Hash1]] instance from an [[Hash2]] instance if we have an
   * [[Hash]] instance for the second type paremeter.
   */
  implicit def hash2ToHash1L[F[_, _], A](implicit F: Hash2[F], A: Hash[A]): Hash1[({ type L[α] = F[α, A] })#L] =
    new Hash1[({ type L[α] = F[α, A] })#L] {
      override def liftHash[B](hash: B => Int, x: F[B, A]): Int =
        F.liftHash2(hash, A.hash, x)

      override def liftEq[B, C](compare: (B, C) => Boolean, x: F[B, A], y: F[C, A]): Boolean =
        F.liftEq2(compare, A.eqv, x, y)
    }

  /**
   * Derive a [[Hash1]] instance from an [[Hash2]] instance if we have an
   * [[Hash]] instance for the first type paremeter.
   */
  implicit def hash2ToHash1R[F[_, _], A](implicit F: Hash2[F], A: Hash[A]): Hash1[({ type L[α] = F[A, α] })#L] =
    new Hash1[({ type L[α] = F[A, α] })#L] {
      override def liftHash[B](hash: B => Int, x: F[A, B]): Int =
        F.liftHash2(A.hash, hash, x)

      override def liftEq[B, C](compare: (B, C) => Boolean, x: F[A, B], y: F[A, C]): Boolean =
        F.liftEq2(A.eqv, compare, x, y)
    }
}
