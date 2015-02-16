package cats
package data

final case class OneAnd[A, F[_]](head: A, tail: F[A]){
  def reduceLeft[B](f: A => B)(g: (B, A) => B)(implicit F: Foldable[F]): B =
    F.foldLeft(tail, f(head))(g)
}

trait OneAndInstances {

  implicit def oneAndShow[A, F[_]](implicit showHead: Show[A], showTail: Show[F[A]]): Show[OneAnd[A, F]] =
    Show.show[OneAnd[A, F]](x => s"OneAnd(${showHead.show(x.head)}, ${showTail.show(x.tail)})")

  implicit def instance[F[_]](implicit monoid: MonoidK[F], functor: Functor[F], monad: Monad[F]): Comonad[OneAnd[?, F]] with Monad[OneAnd[?, F]] = new Comonad[OneAnd[?, F]] with Monad[OneAnd[?, F]] {
    def extract[A](x: OneAnd[A,F]) = x.head

    def coflatMap[A, B](fa: OneAnd[A,F])(f: OneAnd[A,F] => B) =
      OneAnd(f(fa), monoid.empty)

    override def map[A, B](fa: OneAnd[A,F])(f: A => B) =
      OneAnd(f(fa.head), functor.map(fa.tail)(f))

    def pure[A](x: A) = OneAnd(x, monoid.empty)

    private def unwrap[A](fa: OneAnd[A, F]) = monoid.combine(monad.pure(fa.head), fa.tail)

    def flatMap[A, B](fa: OneAnd[A,F])(f: A => OneAnd[B,F]): OneAnd[B,F] = {
      val first = f(fa.head)

      OneAnd(
        first.head,
        monoid.combine(first.tail, monad.flatMap(fa.tail)(a => unwrap(f(a))))
      )
    }
  }

}

object OneAnd extends OneAndInstances
