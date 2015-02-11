package cats
package arrow

final case class Cokleisli[F[_], A, B](run: F[A] => B) { self =>

  def apply(fa: F[A]): B = run(fa)

  def dimap[C, D](f: C => A, g: B => D)(implicit b: Functor[F]): Cokleisli[F, C, D] =
    Cokleisli(fc => g(run(b.map(fc)(f))))

  def contramapValue[C](f: F[C] => F[A]): Cokleisli[F, C,  B] =
    Cokleisli(run compose f)

  def map[C](f: B => C): Cokleisli[F, A, C] =
    Cokleisli(f compose run)

  def flatMap[C](f: B => Cokleisli[F, A, C]): Cokleisli[F, A, C] =
    Cokleisli(fa => f(self.run(fa)).run(fa))

  def compose[C](c: Cokleisli[F, C, A])(implicit F: CoFlatMap[F]): Cokleisli[F, C, B] =
    Cokleisli(fc => run(F.coflatMap(fc)(c.run)))

  def andThen[C](c: Cokleisli[F, B, C])(implicit F: CoFlatMap[F]): Cokleisli[F, A, C] =
    c compose this
}
