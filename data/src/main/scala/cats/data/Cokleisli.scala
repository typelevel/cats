package cats.data

import cats.{CoFlatMap, Functor}

final case class Cokleisli[F[_], A, B](runCokleisli: F[A] => B) { self =>

  def apply(fa: F[A]): B = runCokleisli(fa)

  def dimap[C, D](f: C => A, g: B => D)(implicit b: Functor[F]): Cokleisli[F, C, D] =
    Cokleisli(fc => g(runCokleisli(b.map(fc)(f))))

  def contramapValue[C](f: F[C] => F[A]): Cokleisli[F, C,  B] =
    Cokleisli(runCokleisli compose f)

  def map[C](f: B => C): Cokleisli[F, A, C] =
    Cokleisli(f compose runCokleisli)

  def flatMap[C](f: B => Cokleisli[F, A, C]): Cokleisli[F, A, C] =
    Cokleisli(fa => f(self.runCokleisli(fa)).runCokleisli(fa))

  def compose[C](c: Cokleisli[F, B, C])(implicit F: CoFlatMap[F]): Cokleisli[F, A, C] =
    Cokleisli(fa => c.runCokleisli(F.coflatMap(fa)(runCokleisli)))

  def andThen[C](c: Cokleisli[F, C, A])(implicit F: CoFlatMap[F]): Cokleisli[F, C, B] =
    c compose this
}
