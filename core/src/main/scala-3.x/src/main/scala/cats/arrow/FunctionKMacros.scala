package cats
package arrow

private[arrow] class FunctionKMacroMethods {

  given [F[_], G[_]]: Conversion[F ~> G, [A] => F[A] => G[A]] =
    (f: F ~> G) =>
      [A] => (fa: F[A]) => f(fa)

  given [F[_], G[_]]: Conversion[[A] => F[A] => G[A], F ~> G] =
    (f: [A] => F[A] => G[A]) =>
      new (F ~> G) {
        def apply[A](fa: F[A]): G[A] = f(fa)
      }

}
