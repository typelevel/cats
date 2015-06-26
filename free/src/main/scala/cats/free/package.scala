package cats

package object free {

  /** Alias for the free monad over the `Function0` functor. */
  type Trampoline[A] = Free[Function0, A]
  object Trampoline extends TrampolineFunctions

  /**
   * Free monad of the free functor (Coyoneda) of S.
   *
   * This can be useful because the monad for `Free` requires a functor, and
   * Coyoneda provides a free functor.
   */
  type FreeC[S[_], A] = Free[Coyoneda[S, ?], A]
}
