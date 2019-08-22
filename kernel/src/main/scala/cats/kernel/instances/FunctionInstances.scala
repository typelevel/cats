package cats.kernel
package instances

trait FunctionInstances extends FunctionInstances0 {

  @deprecated("2.0.0-RC2", "Use cats.kernel.instances.function.catsKernelStdOrderForFunction0")
  private[instances] def catsKernelOrderForFunction0[A](implicit ev: Order[A]): Order[() => A] =
    cats.kernel.instances.function.catsKernelStdOrderForFunction0[A]

  @deprecated("2.0.0-RC2", "Use cats.kernel.instances.function.catsKernelStdCommutativeGroupForFunction0")
  private[instances] def catsKernelCommutativeGroupForFunction0[A](
    implicit G: CommutativeGroup[A]
  ): CommutativeGroup[() => A] = cats.kernel.instances.function.catsKernelStdCommutativeGroupForFunction0[A]

  @deprecated("2.0.0-RC2", "Use cats.kernel.instances.function.catsKernelStdCommutativeGroupForFunction1")
  private[instances] def catsKernelCommutativeGroupForFunction1[A, B](
    implicit G: CommutativeGroup[B]
  ): CommutativeGroup[A => B] = cats.kernel.instances.function.catsKernelStdCommutativeGroupForFunction1[A, B]

}

private[instances] trait FunctionInstancesBinCompat0 extends FunctionInstances0BinCompat0 {

  implicit def catsKernelStdOrderForFunction0[A](implicit ev: Order[A]): Order[() => A] =
    new Order[() => A] {
      def compare(x: () => A, y: () => A): Int = ev.compare(x(), y())
    }

  implicit def catsKernelStdCommutativeGroupForFunction0[A](
    implicit G: CommutativeGroup[A]
  ): CommutativeGroup[() => A] =
    new Function0Group[A] with CommutativeGroup[() => A] { def A: Group[A] = G }

  implicit def catsKernelStdCommutativeGroupForFunction1[A, B](
    implicit G: CommutativeGroup[B]
  ): CommutativeGroup[A => B] =
    new Function1Group[A, B] with CommutativeGroup[A => B] { def B: Group[B] = G }
}

private[instances] trait FunctionInstances0 extends FunctionInstances1 {

  @deprecated("2.0.0-RC2", "Use cats.kernel.instances.function.catsKernelStdHashForFunction0")
  private[instances] def catsKernelHashForFunction0[A](implicit ev: Hash[A]): Hash[() => A] =
    cats.kernel.instances.function.catsKernelStdHashForFunction0[A]

  @deprecated("2.0.0-RC2", "Use cats.kernel.instances.function.catsKernelStdPartialOrderForFunction0")
  private[instances] def catsKernelPartialOrderForFunction0[A](implicit ev: PartialOrder[A]): PartialOrder[() => A] =
    cats.kernel.instances.function.catsKernelStdPartialOrderForFunction0[A]

  @deprecated("2.0.0-RC2", "Use cats.kernel.instances.function.catsKernelStdGroupForFunction0")
  private[instances] def catsKernelGroupForFunction0[A](implicit G: Group[A]): Group[() => A] =
    cats.kernel.instances.function.catsKernelStdGroupForFunction0[A]

  @deprecated("2.0.0-RC2", "Use cats.kernel.instances.function.catsKernelStdGroupForFunction1")
  private[instances] def catsKernelGroupForFunction1[A, B](implicit G: Group[B]): Group[A => B] =
    cats.kernel.instances.function.catsKernelStdGroupForFunction1[A, B]

  @deprecated("2.0.0-RC2", "Use cats.kernel.instances.function.catsKernelStdBoundedSemilatticeForFunction0")
  private[instances] def catsKernelBoundedSemilatticeForFunction0[A](
    implicit G: BoundedSemilattice[A]
  ): BoundedSemilattice[() => A] = cats.kernel.instances.function.catsKernelStdBoundedSemilatticeForFunction0[A]

  @deprecated("2.0.0-RC2", "Use cats.kernel.instances.function.catsKernelStdBoundedSemilatticeForFunction1")
  private[instances] def catsKernelBoundedSemilatticeForFunction1[A, B](
    implicit G: BoundedSemilattice[B]
  ): BoundedSemilattice[A => B] = cats.kernel.instances.function.catsKernelStdBoundedSemilatticeForFunction1[A, B]
}

private[instances] trait FunctionInstances0BinCompat0 extends FunctionInstances1BinCompat0 {

  implicit def catsKernelStdHashForFunction0[A](implicit ev: Hash[A]): Hash[() => A] =
    new Hash[() => A] {
      def hash(x: () => A) = ev.hash(x())
      def eqv(x: () => A, y: () => A) = ev.eqv(x(), y())
    }

  implicit def catsKernelStdPartialOrderForFunction0[A](implicit ev: PartialOrder[A]): PartialOrder[() => A] =
    new PartialOrder[() => A] {
      def partialCompare(x: () => A, y: () => A): Double = ev.partialCompare(x(), y())
    }

  implicit def catsKernelStdGroupForFunction0[A](implicit G: Group[A]): Group[() => A] =
    new Function0Group[A] { def A: Group[A] = G }

  implicit def catsKernelStdGroupForFunction1[A, B](implicit G: Group[B]): Group[A => B] =
    new Function1Group[A, B] { def B: Group[B] = G }

  implicit def catsKernelStdBoundedSemilatticeForFunction0[A](
    implicit G: BoundedSemilattice[A]
  ): BoundedSemilattice[() => A] =
    new Function0Monoid[A] with BoundedSemilattice[() => A] { def A: Monoid[A] = G }

  implicit def catsKernelStdBoundedSemilatticeForFunction1[A, B](
    implicit G: BoundedSemilattice[B]
  ): BoundedSemilattice[A => B] =
    new Function1Monoid[A, B] with BoundedSemilattice[A => B] { def B: Monoid[B] = G }
}

private[instances] trait FunctionInstances1 extends FunctionInstances2 {

  @deprecated("2.0.0-RC2", "Use cats.kernel.instances.function.catsKernelStdEqForFunction0")
  private[instances] def catsKernelEqForFunction0[A](implicit ev: Eq[A]): Eq[() => A] =
    cats.kernel.instances.function.catsKernelStdEqForFunction0[A]

  @deprecated("2.0.0-RC2", "Use cats.kernel.instances.function.catsKernelStdCommutativeMonoidForFunction0")
  private[instances] def catsKernelCommutativeMonoidForFunction0[A](
    implicit M: CommutativeMonoid[A]
  ): CommutativeMonoid[() => A] = cats.kernel.instances.function.catsKernelStdCommutativeMonoidForFunction0[A]

  @deprecated("2.0.0-RC2", "Use cats.kernel.instances.function.catsKernelStdCommutativeMonoidForFunction1")
  private[instances] def catsKernelCommutativeMonoidForFunction1[A, B](
    implicit M: CommutativeMonoid[B]
  ): CommutativeMonoid[A => B] = cats.kernel.instances.function.catsKernelStdCommutativeMonoidForFunction1[A, B]

  @deprecated("2.0.0-RC2", "Use cats.kernel.instances.function.catsKernelStdSemilatticeForFunction0")
  private[instances] def catsKernelSemilatticeForFunction0[A](implicit M: Semilattice[A]): Semilattice[() => A] =
    cats.kernel.instances.function.catsKernelStdSemilatticeForFunction0[A]

  @deprecated("2.0.0-RC2", "Use cats.kernel.instances.function.catsKernelStdSemilatticeForFunction1")
  private[instances] def catsKernelSemilatticeForFunction1[A, B](implicit M: Semilattice[B]): Semilattice[A => B] =
    cats.kernel.instances.function.catsKernelStdSemilatticeForFunction1[A, B]
}

private[instances] trait FunctionInstances1BinCompat0 extends FunctionInstances2BinCompat0 {
  implicit def catsKernelStdEqForFunction0[A](implicit ev: Eq[A]): Eq[() => A] =
    new Eq[() => A] {
      def eqv(x: () => A, y: () => A): Boolean = ev.eqv(x(), y())
    }

  implicit def catsKernelStdCommutativeMonoidForFunction0[A](
    implicit M: CommutativeMonoid[A]
  ): CommutativeMonoid[() => A] =
    new Function0Monoid[A] with CommutativeMonoid[() => A] { def A: Monoid[A] = M }

  implicit def catsKernelStdCommutativeMonoidForFunction1[A, B](
    implicit M: CommutativeMonoid[B]
  ): CommutativeMonoid[A => B] =
    new Function1Monoid[A, B] with CommutativeMonoid[A => B] { def B: Monoid[B] = M }

  implicit def catsKernelStdSemilatticeForFunction0[A](implicit M: Semilattice[A]): Semilattice[() => A] =
    new Function0Semigroup[A] with Semilattice[() => A] { def A: Semigroup[A] = M }

  implicit def catsKernelStdSemilatticeForFunction1[A, B](implicit M: Semilattice[B]): Semilattice[A => B] =
    new Function1Semigroup[A, B] with Semilattice[A => B] { def B: Semigroup[B] = M }
}

private[instances] trait FunctionInstances2 extends FunctionInstances3 {

  @deprecated("2.0.0-RC2", "Use cats.kernel.instances.function.catsKernelStdMonoidForFunction0")
  private[instances] def catsKernelMonoidForFunction0[A](implicit M: Monoid[A]): Monoid[() => A] =
    cats.kernel.instances.function.catsKernelStdMonoidForFunction0[A]

  @deprecated("2.0.0-RC2", "Use cats.kernel.instances.function.catsKernelStdMonoidForFunction1")
  private[instances] def catsKernelMonoidForFunction1[A, B](implicit M: Monoid[B]): Monoid[A => B] =
    cats.kernel.instances.function.catsKernelStdMonoidForFunction1[A, B]

  @deprecated("2.0.0-RC2", "Use cats.kernel.instances.function.catsKernelStdBandForFunction0")
  private[instances] def catsKernelBandForFunction0[A](implicit S: Band[A]): Band[() => A] =
    cats.kernel.instances.function.catsKernelStdBandForFunction0[A]

  @deprecated("2.0.0-RC2", "Use cats.kernel.instances.function.catsKernelStdBandForFunction1")
  private[instances] def catsKernelBandForFunction1[A, B](implicit S: Band[B]): Band[A => B] =
    cats.kernel.instances.function.catsKernelStdBandForFunction1[A, B]
}

private[instances] trait FunctionInstances2BinCompat0 extends FunctionInstances3BinCompat0 {

  implicit def catsKernelStdMonoidForFunction0[A](implicit M: Monoid[A]): Monoid[() => A] =
    new Function0Monoid[A] { def A: Monoid[A] = M }

  implicit def catsKernelStdMonoidForFunction1[A, B](implicit M: Monoid[B]): Monoid[A => B] =
    new Function1Monoid[A, B] { def B: Monoid[B] = M }

  implicit def catsKernelStdBandForFunction0[A](implicit S: Band[A]): Band[() => A] =
    new Function0Semigroup[A] with Band[() => A] { def A: Semigroup[A] = S }

  implicit def catsKernelStdBandForFunction1[A, B](implicit S: Band[B]): Band[A => B] =
    new Function1Semigroup[A, B] with Band[A => B] { def B: Semigroup[B] = S }
}

private[instances] trait FunctionInstances3 extends FunctionInstances4 {

  @deprecated("2.0.0-RC2", "Use cats.kernel.instances.function.catsKernelStdCommutativeSemigroupForFunction0")
  private[instances] def catsKernelCommutativeSemigroupForFunction0[A](
    implicit S: CommutativeSemigroup[A]
  ): CommutativeSemigroup[() => A] = cats.kernel.instances.function.catsKernelStdCommutativeSemigroupForFunction0[A]

  @deprecated("2.0.0-RC2", "Use cats.kernel.instances.function.catsKernelStdCommutativeSemigroupForFunction1")
  private[instances] def catsKernelCommutativeSemigroupForFunction1[A, B](
    implicit S: CommutativeSemigroup[B]
  ): CommutativeSemigroup[A => B] = cats.kernel.instances.function.catsKernelStdCommutativeSemigroupForFunction1[A, B]

}

private[instances] trait FunctionInstances3BinCompat0 extends FunctionInstances4BinCompat0 {

  implicit def catsKernelStdCommutativeSemigroupForFunction0[A](
    implicit S: CommutativeSemigroup[A]
  ): CommutativeSemigroup[() => A] =
    new Function0Semigroup[A] with CommutativeSemigroup[() => A] { def A: Semigroup[A] = S }

  implicit def catsKernelStdCommutativeSemigroupForFunction1[A, B](
    implicit S: CommutativeSemigroup[B]
  ): CommutativeSemigroup[A => B] =
    new Function1Semigroup[A, B] with CommutativeSemigroup[A => B] { def B: Semigroup[B] = S }
}

private[instances] trait FunctionInstances4 {

  @deprecated("2.0.0-RC2", "Use cats.kernel.instances.function.catsKernelStdSemigroupForFunction0")
  private[instances] def catsKernelSemigroupForFunction0[A](implicit S: Semigroup[A]): Semigroup[() => A] =
    cats.kernel.instances.function.catsKernelStdSemigroupForFunction0[A]

  @deprecated("2.0.0-RC2", "Use cats.kernel.instances.function.catsKernelStdSemigroupForFunction1")
  private[instances] def catsKernelSemigroupForFunction1[A, B](implicit S: Semigroup[B]): Semigroup[A => B] =
    cats.kernel.instances.function.catsKernelStdSemigroupForFunction1[A, B]
}

private[instances] trait FunctionInstances4BinCompat0 {

  implicit def catsKernelStdSemigroupForFunction0[A](implicit S: Semigroup[A]): Semigroup[() => A] =
    new Function0Semigroup[A] { def A: Semigroup[A] = S }

  implicit def catsKernelStdSemigroupForFunction1[A, B](implicit S: Semigroup[B]): Semigroup[A => B] =
    new Function1Semigroup[A, B] { def B: Semigroup[B] = S }
}

trait Function1Semigroup[A, B] extends Semigroup[A => B] {
  implicit def B: Semigroup[B]

  override def combine(x: A => B, y: A => B): A => B =
    (a: A) => B.combine(x(a), y(a))
}

trait Function1Monoid[A, B] extends Function1Semigroup[A, B] with Monoid[A => B] {
  implicit def B: Monoid[B]

  val empty: A => B =
    (_: A) => B.empty
}

trait Function1Group[A, B] extends Function1Monoid[A, B] with Group[A => B] {
  implicit def B: Group[B]

  def inverse(x: A => B): A => B =
    (a: A) => B.inverse(x(a))
}

trait Function0Semigroup[A] extends Semigroup[() => A] {
  implicit def A: Semigroup[A]

  override def combine(x: () => A, y: () => A): () => A =
    () => A.combine(x(), y())
}

trait Function0Monoid[A] extends Function0Semigroup[A] with Monoid[() => A] {
  implicit def A: Monoid[A]

  val empty: () => A =
    () => A.empty
}

trait Function0Group[A] extends Function0Monoid[A] with Group[() => A] {
  implicit def A: Group[A]

  def inverse(x: () => A): () => A =
    () => A.inverse(x())
}
