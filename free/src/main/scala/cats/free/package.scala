package cats

package object free {
  /** Alias for the free monad over the `Function0` functor. */
  type Trampoline[A] = Free[Function0, A]
  object Trampoline extends TrampolineFunctions
  type StacksafeKleisli[F[_], A, B] = FreeT[A => ?, F, B]

}
