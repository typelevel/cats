package cats
package data

/**
 * A data type which represents a single element (head) and some other
 * structure (tail). As we have done in package.scala, this can be
 * used to represent a List which is guaranteed to not be empty:
 * {{{
 * type NonEmptyList[A] = OneAnd[A, List]
 * }}}
 */
final case class OneAnd[A, F[_]](head: A, tail: F[A]) {

  /**
   * remove elements not matching the predicate
   */
  def filter(f: A => Boolean)(implicit monad: MonadCombine[F]): F[A] = {
    val rest = monad.filter(tail)(f)
    if(f(head))
      monad.combine(monad.pure(head), rest)
    else
      rest
  }

  def combine(other: OneAnd[A,F])(implicit monad: MonadCombine[F]): OneAnd[A,F] =
    OneAnd(head, monad.combine(tail, monad.combine(monad.pure(other.head), other.tail)))

  def foldLeft[B](b: B)(f: (B, A) => B)(implicit foldable: Foldable[F]): B =
    foldable.foldLeft(tail, f(b, head))(f)

  def foldRight[B](b: B)(f: (A,B) => B)(implicit foldable: Foldable[F]): B =
    f(head, foldable.foldRight(tail, b)(f))

  def foldLazy[B](b: Lazy[B])(f: A => Fold[B])(implicit foldable: Foldable[F]): Lazy[B] = {
    import Fold._
    f(head) match {
      case Return(b) => Lazy.eager(b)
      case Continue(c) => foldable.foldLazy(tail, b)(f).map(c)
      case _ => foldable.foldLazy(tail, b)(f)
    }
  }
}

trait OneAndInstances {

  implicit def oneAndShow[A, F[_]](implicit showHead: Show[A], showTail: Show[F[A]]): Show[OneAnd[A, F]] =
    Show.show[OneAnd[A, F]](x => s"OneAnd(${showHead.show(x.head)}, ${showTail.show(x.tail)})")

  implicit def oneAndFunctor[F[_]](F: Functor[F]) = new Functor[OneAnd[?,F]] {
    override def map[A, B](fa: OneAnd[A,F])(f: A => B) =
      OneAnd(f(fa.head), F.map(fa.tail)(f))
  }

  implicit def oneAndSemigroupK[F[_]](implicit monad: MonadCombine[F]): SemigroupK[OneAnd[?,F]] = new SemigroupK[OneAnd[?,F]] {
    def combine[A](a: OneAnd[A, F], b: OneAnd[A, F]) = a combine b
  }

  implicit def onAndFoldable[F[_]](implicit foldable: Foldable[F]): Foldable[OneAnd[?,F]] = new Foldable[OneAnd[?,F]] {
    override def foldLeft[A,B](fa: OneAnd[A,F], b: B)(f: (B,A) => B): B =
      fa.foldLeft(b)(f)
    override def foldRight[A,B](fa: OneAnd[A,F], b: B)(f: (A,B) => B): B =
      fa.foldRight(b)(f)
    override def foldLazy[A,B](fa: OneAnd[A,F], b: Lazy[B])(f: A => Fold[B]): Lazy[B] =
      fa.foldLazy(b)(f)
  }

  implicit def oneAndMonad[F[_]](implicit monad: MonadCombine[F]): Comonad[OneAnd[?, F]] with Monad[OneAnd[?, F]] = new Comonad[OneAnd[?, F]] with Monad[OneAnd[?, F]] {
    def extract[A](x: OneAnd[A,F]) = x.head

    def coflatMap[A, B](fa: OneAnd[A,F])(f: OneAnd[A,F] => B) =
      OneAnd(f(fa), monad.empty)

    override def map[A, B](fa: OneAnd[A,F])(f: A => B) =
      OneAnd(f(fa.head), monad.map(fa.tail)(f))

    def pure[A](x: A) = OneAnd(x, monad.empty)

    private def unwrap[A](fa: OneAnd[A, F]) = monad.combine(monad.pure(fa.head), fa.tail)

    def flatMap[A, B](fa: OneAnd[A,F])(f: A => OneAnd[B,F]): OneAnd[B,F] = {
      val first = f(fa.head)

      OneAnd(
        first.head,
        monad.combine(first.tail, monad.flatMap(fa.tail)(a => unwrap(f(a))))
      )
    }
  }

}

object OneAnd extends OneAndInstances
