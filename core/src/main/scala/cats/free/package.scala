package cats

package object free {

  /** Alias for the free monad over the `Function0` functor. */
  type Trampoline[A] = Free[Function0, A]

  type FreeC[S[_], A] = Free[Coyoneda[S, ?], A]
}
