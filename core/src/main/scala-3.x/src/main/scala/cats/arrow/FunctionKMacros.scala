package cats
package arrow

private[arrow] class FunctionKMacroMethods:
  /**
   * Lifts function `f` of `[X] => F[X] => G[X]` into a `FunctionK[F, G]`.
   *
   * {{{
   *   val headOptionK = FunctionK.lift[List, Option]([X] => (_: List[X]).headOption)
   * }}}
   */
  def lift[F[_], G[_]](f: [X] => F[X] => G[X]): FunctionK[F, G] =
    new FunctionK[F, G]:
      def apply[A](fa: F[A]): G[A] = f(fa)
