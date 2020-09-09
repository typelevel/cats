package cats
package free

/**
 * A free comonad for some branching functor `S`. Branching is done lazily using [[Eval]].
 * A tree with data at the branches, as opposed to [[Free]] which is a tree with data at the leaves.
 * Not an instruction set functor made into a program monad as in [[Free]], but an instruction set's outputs as a
 * functor made into a tree of the possible worlds reachable using the instruction set.
 *
 * This Scala implementation of `Cofree` and its usages are derived from
 * [[https://github.com/scalaz/scalaz/blob/series/7.3.x/core/src/main/scala/scalaz/Cofree.scala Scalaz's Cofree]],
 * originally written by Rúnar Bjarnason.
 */
final case class Cofree[S[_], A](head: A, tail: Eval[S[Cofree[S, A]]]) {

  /**
   * Evaluates and returns the tail of the computation.
   */
  def tailForced: S[Cofree[S, A]] = tail.value

  /**
   * Applies `f` to the head and `g` to the tail.
   */
  def transform[B](f: A => B, g: Cofree[S, A] => Cofree[S, B])(implicit S: Functor[S]): Cofree[S, B] =
    Cofree[S, B](f(head), tail.map(S.map(_)(g)))

  /**
   * Map over head and inner `S[_]` branches.
   */
  def map[B](f: A => B)(implicit S: Functor[S]): Cofree[S, B] =
    transform(f, _.map(f))

  /**
   * Transform the branching functor at the root of the Cofree tree.
   */
  def mapBranchingRoot(nat: S ~> S)(implicit S: Functor[S]): Cofree[S, A] =
    Cofree[S, A](head, tail.map(nat(_)))

  /**
   * Transform the branching functor, using the S functor to perform the recursion.
   */
  def mapBranchingS[T[_]](nat: S ~> T)(implicit S: Functor[S]): Cofree[T, A] =
    Cofree[T, A](head, tail.map(v => nat(S.map(v)(_.mapBranchingS(nat)))))

  /**
   * Transform the branching functor, using the T functor to perform the recursion.
   */
  def mapBranchingT[T[_]](nat: S ~> T)(implicit T: Functor[T]): Cofree[T, A] =
    Cofree.anaEval(this)(_.tail.map(nat(_)), _.head)

  /**
   * Map `f` over each subtree of the computation.
   */
  def coflatMap[B](f: Cofree[S, A] => B)(implicit S: Functor[S]): Cofree[S, B] =
    Cofree.anaEval(this)(_.tail, f)

  /**
   * Replace each node in the computation with the subtree from that node downwards
   */
  def coflatten(implicit S: Functor[S]): Cofree[S, Cofree[S, A]] =
    Cofree.anaEval(this)(_.tail, identity)

  /**
   * Alias for head.
   */
  def extract: A = head

  /**
   * Evaluate just the tail.
   */
  def forceTail: Cofree[S, A] =
    Cofree[S, A](head, Eval.now(tail.value))

  /**
   * Evaluate the entire Cofree tree.
   */
  def forceAll(implicit S: Functor[S]): Cofree[S, A] =
    Cofree.anaEval(this)(sa => Eval.now(sa.tail.value), _.head)

}

object Cofree extends CofreeInstances {

  /**
   * Cofree anamorphism, lazily evaluated.
   */
  def unfold[F[_], A](a: A)(f: A => F[A])(implicit F: Functor[F]): Cofree[F, A] =
    ana(a)(f, identity)

  /**
   * Cofree anamorphism with a fused map, lazily evaluated.
   */
  def ana[F[_], A, B](a: A)(coalg: A => F[A], f: A => B)(implicit F: Functor[F]): Cofree[F, B] =
    anaEval(a)(a => Eval.later(coalg(a)), f)

  /**
   * Cofree anamorphism with a fused map.
   */
  def anaEval[F[_], A, B](a: A)(coalg: A => Eval[F[A]], f: A => B)(implicit F: Functor[F]): Cofree[F, B] =
    Cofree[F, B](f(a), mapSemilazy(coalg(a))(fa => F.map(fa)(anaEval(_)(coalg, f))))

  private def mapSemilazy[A, B](fa: Eval[A])(f: A => B): Eval[B] =
    fa match {
      case Now(a) => Now(f(a))
      case other  => other.map(f)
    }

  /**
   * A stack-safe algebraic recursive fold out of the cofree comonad.
   */
  def cata[F[_], A, B](cof: Cofree[F, A])(folder: (A, F[B]) => Eval[B])(implicit F: Traverse[F]): Eval[B] =
    F.traverse(cof.tailForced)(c => Eval.defer(cata(c)(folder))).flatMap(folder(cof.head, _))

  /**
   * A monadic recursive fold out of the cofree comonad into a monad which can express Eval's stack-safety.
   */
  def cataM[F[_], M[_], A, B](
    cof: Cofree[F, A]
  )(folder: (A, F[B]) => M[B])(inclusion: Eval ~> M)(implicit F: Traverse[F], M: Monad[M]): M[B] = {
    def loop(fr: Cofree[F, A]): Eval[M[B]] = {
      val looped: M[F[B]] =
        F.traverse[M, Cofree[F, A], B](fr.tailForced)(fr => M.flatten(inclusion(Eval.defer(loop(fr)))))
      val folded: M[B] = M.flatMap(looped)(fb => folder(fr.head, fb))
      Eval.now(folded)
    }
    M.flatten(inclusion(loop(cof)))
  }

}

sealed abstract private[free] class CofreeInstances2 {
  implicit def catsReducibleForCofree[F[_]: Foldable]: Reducible[Cofree[F, *]] =
    new CofreeReducible[F] {
      def F = implicitly
    }
}

sealed abstract private[free] class CofreeInstances1 extends CofreeInstances2 {
  implicit def catsTraverseForCofree[F[_]: Traverse]: Traverse[Cofree[F, *]] =
    new CofreeTraverse[F] {
      def F = implicitly
    }
}

sealed abstract private[free] class CofreeInstances extends CofreeInstances1 {
  implicit def catsFreeComonadForCofree[S[_]: Functor]: Comonad[Cofree[S, *]] =
    new CofreeComonad[S] {
      def F = implicitly
    }
}

private trait CofreeComonad[S[_]] extends Comonad[Cofree[S, *]] {
  implicit def F: Functor[S]

  final override def extract[A](p: Cofree[S, A]): A = p.extract

  final override def coflatMap[A, B](a: Cofree[S, A])(f: Cofree[S, A] => B): Cofree[S, B] = a.coflatMap(f)

  final override def coflatten[A](a: Cofree[S, A]): Cofree[S, Cofree[S, A]] = a.coflatten

  final override def map[A, B](a: Cofree[S, A])(f: A => B): Cofree[S, B] = a.map(f)
}

private trait CofreeReducible[F[_]] extends Reducible[Cofree[F, *]] {
  implicit def F: Foldable[F]

  final override def foldMap[A, B](fa: Cofree[F, A])(f: A => B)(implicit M: Monoid[B]): B =
    M.combine(f(fa.head), F.foldMap(fa.tailForced)(foldMap(_)(f)))

  final override def foldRight[A, B](fa: Cofree[F, A], z: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    f(fa.head, fa.tail.flatMap(F.foldRight(_, z)(foldRight(_, _)(f))))

  final override def foldLeft[A, B](fa: Cofree[F, A], z: B)(f: (B, A) => B): B =
    F.foldLeft(fa.tailForced, f(z, fa.head))((b, cof) => foldLeft(cof, b)(f))

  final override def reduceLeftTo[A, B](fa: Cofree[F, A])(z: A => B)(f: (B, A) => B): B =
    F.foldLeft(fa.tailForced, z(fa.head))((b, cof) => foldLeft(cof, b)(f))

  override def reduceRightTo[A, B](fa: Cofree[F, A])(z: A => B)(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    foldRight(fa, Eval.now((None: Option[B]))) { case (l, e) =>
      e.flatMap {
        case None    => Eval.now(Some(z(l)))
        case Some(r) => f(l, Eval.now(r)).map(Some(_))
      }
    }.map(_.getOrElse(sys.error("reduceRightTo")))

}

private trait CofreeTraverse[F[_]] extends Traverse[Cofree[F, *]] with CofreeReducible[F] with CofreeComonad[F] {
  implicit def F: Traverse[F]

  final override def traverse[G[_], A, B](fa: Cofree[F, A])(f: A => G[B])(implicit G: Applicative[G]): G[Cofree[F, B]] =
    G.map2(f(fa.head), F.traverse(fa.tailForced)(traverse(_)(f)))((h, t) => Cofree[F, B](h, Eval.now(t)))

}
