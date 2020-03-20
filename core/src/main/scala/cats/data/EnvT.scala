package cats.data

import cats.{Applicative, Comonad, Eval, Foldable, Functor, Monoid, Traverse}

/**
 * EnvT[W, E, A] is the Env Comonad transformer
 */
final case class EnvT[W[_], E, A](wa: W[A], env: E) {

  /**
   * Return the environment
   */
  def ask: E = env

  /**
   * Return a transformed environment
   */
  def asks[B](f: E => B): B = f(env)

  /**
   * Transform the environment
   */
  def local[E1](f: E => E1): EnvT[W, E1, A] = EnvT(wa, f(env))

  def map[B](f: A => B)(implicit W: Functor[W]): EnvT[W, E, B] = EnvT(W.map(wa)(f), env)

  def runEnvT: (W[A], E) = (wa, env)
}

object EnvT {

  implicit def catsDataEnvTFunctor[W[_]: Functor, E]: Functor[EnvT[W, E, *]] = new Functor[EnvT[W, E, *]] {
    override def map[A, B](fa: EnvT[W, E, A])(f: A => B): EnvT[W, E, B] = fa.map(f)
  }

  implicit def catsDataEnvTApplicative[W[_]: Applicative, E: Monoid]: Applicative[EnvT[W, E, *]] =
    new Applicative[EnvT[W, E, *]] {
      override def pure[A](x: A): EnvT[W, E, A] = EnvT(Applicative[W].pure(x), Monoid[E].empty)

      override def ap[A, B](ff: EnvT[W, E, A => B])(fa: EnvT[W, E, A]): EnvT[W, E, B] =
        EnvT(Applicative[W].ap(ff.wa)(fa.wa), Monoid[E].combine(ff.env, fa.env))
    }

  implicit def catsDataEnvTComonad[W[_]: Comonad, E]: Comonad[EnvT[W, E, *]] = new Comonad[EnvT[W, E, *]] {
    override def extract[A](x: EnvT[W, E, A]): A = Comonad[W].extract(x.wa)

    override def coflatMap[A, B](fa: EnvT[W, E, A])(f: EnvT[W, E, A] => B): EnvT[W, E, B] =
      EnvT(Comonad[W].coflatMap(fa.wa)(wa => f(EnvT(wa, fa.env))), fa.env)

    override def map[A, B](fa: EnvT[W, E, A])(f: A => B): EnvT[W, E, B] = catsDataEnvTFunctor[W, E].map(fa)(f)
  }

  implicit def catsDataEnvTFoldable[W[_]: Foldable, E]: Foldable[EnvT[W, E, *]] = new Foldable[EnvT[W, E, *]] {
    override def foldLeft[A, B](fa: EnvT[W, E, A], b: B)(f: (B, A) => B): B = Foldable[W].foldLeft(fa.wa, b)(f)

    override def foldRight[A, B](fa: EnvT[W, E, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      Foldable[W].foldRight(fa.wa, lb)(f)
  }

  implicit def catsDataEnvTTraverse[W[_]: Traverse, E]: Traverse[EnvT[W, E, *]] = new Traverse[EnvT[W, E, *]] {
    override def traverse[G[_]: Applicative, A, B](fa: EnvT[W, E, A])(f: A => G[B]): G[EnvT[W, E, B]] =
      Functor[G].map(Traverse[W].traverse(fa.wa)(f))(EnvT(_, fa.env))

    override def foldLeft[A, B](fa: EnvT[W, E, A], b: B)(f: (B, A) => B): B =
      catsDataEnvTFoldable[W, E].foldLeft(fa, b)(f)

    override def foldRight[A, B](fa: EnvT[W, E, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      catsDataEnvTFoldable[W, E].foldRight(fa, lb)(f)
  }
}
