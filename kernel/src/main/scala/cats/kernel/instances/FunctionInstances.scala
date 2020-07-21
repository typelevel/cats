package cats.kernel
package instances

trait FunctionInstances extends FunctionInstances0 {

  implicit def catsKernelOrderForFunction0[A](implicit ev: Order[A]): Order[() => A] =
    new Order[() => A] {
      def compare(x: () => A, y: () => A): Int = ev.compare(x(), y())
    }

  implicit def catsKernelCommutativeGroupForFunction0[A](implicit G: CommutativeGroup[A]): CommutativeGroup[() => A] =
    new Function0Group[A] with CommutativeGroup[() => A] { def A: Group[A] = G }

  implicit def catsKernelCommutativeGroupForFunction1[A, B](implicit G: CommutativeGroup[B]): CommutativeGroup[A => B] =
    new Function1Group[A, B] with CommutativeGroup[A => B] { def B: Group[B] = G }
}

private[instances] trait FunctionInstances0 extends FunctionInstances1 {

  implicit def catsKernelHashForFunction0[A](implicit ev: Hash[A]): Hash[() => A] =
    new Hash[() => A] {
      def hash(x: () => A) = ev.hash(x())
      def eqv(x: () => A, y: () => A) = ev.eqv(x(), y())
    }

  implicit def catsKernelPartialOrderForFunction0[A](implicit ev: PartialOrder[A]): PartialOrder[() => A] =
    new PartialOrder[() => A] {
      def partialCompare(x: () => A, y: () => A): Double = ev.partialCompare(x(), y())
    }

  implicit def catsKernelGroupForFunction0[A](implicit G: Group[A]): Group[() => A] =
    new Function0Group[A] { def A: Group[A] = G }

  implicit def catsKernelGroupForFunction1[A, B](implicit G: Group[B]): Group[A => B] =
    new Function1Group[A, B] { def B: Group[B] = G }

  implicit def catsKernelBoundedSemilatticeForFunction0[A](implicit
    G: BoundedSemilattice[A]
  ): BoundedSemilattice[() => A] =
    new Function0Monoid[A] with BoundedSemilattice[() => A] { def A: Monoid[A] = G }

  implicit def catsKernelBoundedSemilatticeForFunction1[A, B](implicit
    G: BoundedSemilattice[B]
  ): BoundedSemilattice[A => B] =
    new Function1Monoid[A, B] with BoundedSemilattice[A => B] { def B: Monoid[B] = G }
}

private[instances] trait FunctionInstances1 extends FunctionInstances2 {

  implicit def catsKernelEqForFunction0[A](implicit ev: Eq[A]): Eq[() => A] =
    new Eq[() => A] {
      def eqv(x: () => A, y: () => A): Boolean = ev.eqv(x(), y())
    }

  implicit def catsKernelCommutativeMonoidForFunction0[A](implicit
    M: CommutativeMonoid[A]
  ): CommutativeMonoid[() => A] =
    new Function0Monoid[A] with CommutativeMonoid[() => A] { def A: Monoid[A] = M }

  implicit def catsKernelCommutativeMonoidForFunction1[A, B](implicit
    M: CommutativeMonoid[B]
  ): CommutativeMonoid[A => B] =
    new Function1Monoid[A, B] with CommutativeMonoid[A => B] { def B: Monoid[B] = M }

  implicit def catsKernelSemilatticeForFunction0[A](implicit M: Semilattice[A]): Semilattice[() => A] =
    new Function0Semigroup[A] with Semilattice[() => A] { def A: Semigroup[A] = M }

  implicit def catsKernelSemilatticeForFunction1[A, B](implicit M: Semilattice[B]): Semilattice[A => B] =
    new Function1Semigroup[A, B] with Semilattice[A => B] { def B: Semigroup[B] = M }
}

private[instances] trait FunctionInstances2 extends FunctionInstances3 {

  implicit def catsKernelMonoidForFunction0[A](implicit M: Monoid[A]): Monoid[() => A] =
    new Function0Monoid[A] { def A: Monoid[A] = M }

  implicit def catsKernelMonoidForFunction1[A, B](implicit M: Monoid[B]): Monoid[A => B] =
    new Function1Monoid[A, B] { def B: Monoid[B] = M }

  implicit def catsKernelBandForFunction0[A](implicit S: Band[A]): Band[() => A] =
    new Function0Semigroup[A] with Band[() => A] { def A: Semigroup[A] = S }

  implicit def catsKernelBandForFunction1[A, B](implicit S: Band[B]): Band[A => B] =
    new Function1Semigroup[A, B] with Band[A => B] { def B: Semigroup[B] = S }
}

private[instances] trait FunctionInstances3 extends FunctionInstances4 {

  implicit def catsKernelCommutativeSemigroupForFunction0[A](implicit
    S: CommutativeSemigroup[A]
  ): CommutativeSemigroup[() => A] =
    new Function0Semigroup[A] with CommutativeSemigroup[() => A] { def A: Semigroup[A] = S }

  implicit def catsKernelCommutativeSemigroupForFunction1[A, B](implicit
    S: CommutativeSemigroup[B]
  ): CommutativeSemigroup[A => B] =
    new Function1Semigroup[A, B] with CommutativeSemigroup[A => B] { def B: Semigroup[B] = S }
}

private[instances] trait FunctionInstances4 {

  implicit def catsKernelSemigroupForFunction0[A](implicit S: Semigroup[A]): Semigroup[() => A] =
    new Function0Semigroup[A] { def A: Semigroup[A] = S }

  implicit def catsKernelSemigroupForFunction1[A, B](implicit S: Semigroup[B]): Semigroup[A => B] =
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
