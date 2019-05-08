package cats
package free

import scala.annotation.tailrec

import cats.arrow.FunctionK

/**
 * A free operational monad for some functor `S`. Binding is done
 * using the heap instead of the stack, allowing tail-call
 * elimination.
 */
sealed abstract class Free[S[_], A] extends Product with Serializable {

  import Free.{FlatMapped, Pure, Suspend}

  final def map[B](f: A => B): Free[S, B] =
    flatMap(a => Pure(f(a)))

  /**
   * Modify the functor context `S` using transformation `f`.
   *
   * This is effectively compiling your free monad into another
   * language by changing the suspension functor using the given
   * natural transformation `f`.
   *
   * If your natural transformation is effectful, be careful. These
   * effects will be applied by `mapK`.
   */
  final def mapK[T[_]](f: S ~> T): Free[T, A] =
    foldMap[Free[T, ?]] { // this is safe because Free is stack safe
      λ[FunctionK[S, Free[T, ?]]](fa => Suspend(f(fa)))
    }

  /**
   * Bind the given continuation to the result of this computation.
   * All left-associated binds are reassociated to the right.
   */
  final def flatMap[B](f: A => Free[S, B]): Free[S, B] =
    FlatMapped(this, f)

  /**
   * Catamorphism. Run the first given function if Pure, otherwise,
   * the second given function.
   */
  final def fold[B](r: A => B, s: S[Free[S, A]] => B)(implicit S: Functor[S]): B =
    resume.fold(s, r)

  /** Takes one evaluation step in the Free monad, re-associating left-nested binds in the process. */
  @tailrec
  final def step: Free[S, A] = this match {
    case FlatMapped(FlatMapped(c, f), g) => c.flatMap(cc => f(cc).flatMap(g)).step
    case FlatMapped(Pure(a), f)          => f(a).step
    case x                               => x
  }

  /**
   * Evaluate a single layer of the free monad.
   */
  @tailrec
  final def resume(implicit S: Functor[S]): Either[S[Free[S, A]], A] = this match {
    case Pure(a)    => Right(a)
    case Suspend(t) => Left(S.map(t)(Pure(_)))
    case FlatMapped(c, f) =>
      c match {
        case Pure(a)          => f(a).resume
        case Suspend(t)       => Left(S.map(t)(f))
        case FlatMapped(d, g) => d.flatMap(dd => g(dd).flatMap(f)).resume
      }
  }

  /**
   * A combination of step and fold.
   */
  final private[free] def foldStep[B](
    onPure: A => B,
    onSuspend: S[A] => B,
    onFlatMapped: ((S[X], X => Free[S, A]) forSome { type X }) => B
  ): B = this.step match {
    case Pure(a)                    => onPure(a)
    case Suspend(a)                 => onSuspend(a)
    case FlatMapped(Suspend(fa), f) => onFlatMapped((fa, f))
    case _                          => sys.error("FlatMapped should be right associative after step")
  }

  /**
   * Run to completion, using a function that extracts the resumption
   * from its suspension functor.
   */
  final def go(f: S[Free[S, A]] => Free[S, A])(implicit S: Functor[S]): A = {
    @tailrec def loop(t: Free[S, A]): A =
      t.resume match {
        case Left(s)  => loop(f(s))
        case Right(r) => r
      }
    loop(this)
  }

  /**
   * Run to completion, using the given comonad to extract the
   * resumption.
   */
  final def run(implicit S: Comonad[S]): A =
    go(S.extract)

  /**
   * Run to completion, using a function that maps the resumption
   * from `S` to a monad `M`.
   */
  final def runM[M[_]](f: S[Free[S, A]] => M[Free[S, A]])(implicit S: Functor[S], M: Monad[M]): M[A] = {
    def step(t: S[Free[S, A]]): M[Either[S[Free[S, A]], A]] =
      M.map(f(t))(_.resume)

    resume match {
      case Left(s)  => M.tailRecM(s)(step)
      case Right(r) => M.pure(r)
    }
  }

  /**
   * Run to completion, using monadic recursion to evaluate the
   * resumption in the context of `S`.
   */
  final def runTailRec(implicit S: Monad[S]): S[A] = {
    def step(rma: Free[S, A]): S[Either[Free[S, A], A]] =
      rma match {
        case Pure(a) =>
          S.pure(Right(a))
        case Suspend(ma) =>
          S.map(ma)(Right(_))
        case FlatMapped(curr, f) =>
          curr match {
            case Pure(x) =>
              S.pure(Left(f(x)))
            case Suspend(mx) =>
              S.map(mx)(x => Left(f(x)))
            case FlatMapped(prev, g) =>
              S.pure(Left(prev.flatMap(w => g(w).flatMap(f))))
          }
      }
    S.tailRecM(this)(step)
  }

  /**
   * Catamorphism for `Free`.
   *
   * Run to completion, mapping the suspension with the given
   * transformation at each step and accumulating into the monad `M`.
   *
   * This method uses `tailRecM` to provide stack-safety.
   */
  final def foldMap[M[_]](f: FunctionK[S, M])(implicit M: Monad[M]): M[A] =
    M.tailRecM(this)(_.step match {
      case Pure(a)          => M.pure(Right(a))
      case Suspend(sa)      => M.map(f(sa))(Right(_))
      case FlatMapped(c, g) => M.map(c.foldMap(f))(cc => Left(g(cc)))
    })

  /**
   * Compile your free monad into another language by changing the
   * suspension functor using the given natural transformation `f`.
   *
   * If your natural transformation is effectful, be careful. These
   * effects will be applied by `compile`.
   */
  final def compile[T[_]](f: FunctionK[S, T]): Free[T, A] = mapK(f)

  /**
   * Lift into `G` (typically a `EitherK`) given `InjectK`. Analogous
   * to `Free.inject` but lifts programs rather than constructors.
   *
   *{{{
   *scala> type Lo[A] = cats.data.EitherK[List, Option, A]
   *defined type alias Lo
   *
   *scala> val fo = Free.liftF(Option("foo"))
   *fo: cats.free.Free[Option,String] = Free(...)
   *
   *scala> fo.inject[Lo]
   *res4: cats.free.Free[Lo,String] = Free(...)
   *}}}
   */
  final def inject[G[_]](implicit ev: InjectK[S, G]): Free[G, A] =
    mapK(λ[S ~> G](ev.inj(_)))

  final def toFreeT[G[_]: Applicative]: FreeT[S, G, A] =
    foldMap[FreeT[S, G, ?]](λ[S ~> FreeT[S, G, ?]](FreeT.liftF(_)))

  override def toString: String =
    "Free(...)"
}

object Free extends FreeInstances {

  /**
   * Return from the computation with the given value.
   */
  final private[free] case class Pure[S[_], A](a: A) extends Free[S, A]

  /** Suspend the computation with the given suspension. */
  final private[free] case class Suspend[S[_], A](a: S[A]) extends Free[S, A]

  /** Call a subroutine and continue with the given function. */
  final private[free] case class FlatMapped[S[_], B, C](c: Free[S, C], f: C => Free[S, B]) extends Free[S, B]

  /**
   * Lift a pure `A` value into the free monad.
   */
  def pure[S[_], A](a: A): Free[S, A] = Pure(a)

  /**
   * Lift an `F[A]` value into the free monad.
   */
  def liftF[F[_], A](value: F[A]): Free[F, A] = Suspend(value)

  /**
   * Absorb a step into the free monad.
   */
  def roll[F[_], A](value: F[Free[F, A]]): Free[F, A] =
    liftF(value).flatMap(identity)

  /**
   * Suspend the creation of a `Free[F, A]` value.
   */
  @deprecated("Use Free.defer.", "1.0.0-MF")
  def suspend[F[_], A](value: => Free[F, A]): Free[F, A] =
    defer(value)

  /**
   * Defer the creation of a `Free[F, A]` value.
   */
  def defer[F[_], A](value: => Free[F, A]): Free[F, A] =
    pure(()).flatMap(_ => value)

  /**
   * a FunctionK, suitable for composition, which calls mapK
   */
  def mapK[F[_], G[_]](fk: FunctionK[F, G]): FunctionK[Free[F, ?], Free[G, ?]] =
    λ[FunctionK[Free[F, ?], Free[G, ?]]](f => f.mapK(fk))

  /**
   * a FunctionK, suitable for composition, which calls compile
   */
  def compile[F[_], G[_]](fk: FunctionK[F, G]): FunctionK[Free[F, ?], Free[G, ?]] =
    mapK(fk)

  /**
   * a FunctionK, suitable for composition, which calls foldMap
   */
  def foldMap[F[_], M[_]: Monad](fk: FunctionK[F, M]): FunctionK[Free[F, ?], M] =
    λ[FunctionK[Free[F, ?], M]](f => f.foldMap(fk))

  /**
   * This method is used to defer the application of an InjectK[F, G]
   * instance. The actual work happens in
   * `FreeInjectKPartiallyApplied#apply`.
   *
   * This method exists to allow the `F` and `G` parameters to be
   * bound independently of the `A` parameter below.
   */
  // TODO to be deprecated / removed in cats 2.0
  def inject[F[_], G[_]]: FreeInjectKPartiallyApplied[F, G] =
    new FreeInjectKPartiallyApplied

  /**
   * Uses the [[http://typelevel.org/cats/guidelines.html#partially-applied-type-params Partially Applied Type Params technique]] for ergonomics.
   */
  final private[free] class FreeInjectKPartiallyApplied[F[_], G[_]](private val dummy: Boolean = true) extends AnyVal {
    def apply[A](fa: F[A])(implicit I: InjectK[F, G]): Free[G, A] =
      Free.liftF(I.inj(fa))
  }

  /**
   * This method is used to defer the application of an InjectK[F, G]
   * instance. The actual work happens in
   * `FreeLiftInjectKPartiallyApplied#apply`.
   *
   * This method exists to allow the `G` parameter to be
   * bound independently of the `F` and `A` parameters below.
   */
  def liftInject[G[_]]: FreeLiftInjectKPartiallyApplied[G] =
    new FreeLiftInjectKPartiallyApplied

  /**
   * Uses the [[http://typelevel.org/cats/guidelines.html#partially-applied-type-params Partially Applied Type Params technique]] for ergonomics.
   */
  final private[free] class FreeLiftInjectKPartiallyApplied[G[_]](private val dummy: Boolean = true) extends AnyVal {
    def apply[F[_], A](fa: F[A])(implicit I: InjectK[F, G]): Free[G, A] =
      Free.liftF(I.inj(fa))
  }

  def injectRoll[F[_], G[_], A](ga: G[Free[F, A]])(implicit I: InjectK[G, F]): Free[F, A] =
    Free.roll(I.inj(ga))

  def match_[F[_], G[_], A](fa: Free[F, A])(implicit F: Functor[F], I: InjectK[G, F]): Option[G[Free[F, A]]] =
    fa.resume.fold(I.prj(_), _ => None)

  implicit def catsFreeMonadForId: Monad[Free[Id, ?]] = catsFreeMonadForFree[Id]

  implicit def catsFreeDeferForId: Defer[Free[Id, ?]] = catsFreeDeferForFree[Id]
}

private trait FreeFoldable[F[_]] extends Foldable[Free[F, ?]] {

  implicit def F: Foldable[F]

  final override def foldLeft[A, B](fa: Free[F, A], b: B)(f: (B, A) => B): B =
    fa.foldStep(
      a => f(b, a),
      fa => F.foldLeft(fa, b)(f),
      { case (fx, g) => F.foldLeft(fx, b)((bb, x) => foldLeft(g(x), bb)(f)) }
    )

  final override def foldRight[A, B](fa: Free[F, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    fa.foldStep(
      a => f(a, lb),
      fa => F.foldRight(fa, lb)(f),
      { case (fx, g) => F.foldRight(fx, lb)((a, lbb) => foldRight(g(a), lbb)(f)) }
    )
}

private trait FreeTraverse[F[_]] extends Traverse[Free[F, ?]] with FreeFoldable[F] {
  implicit def TraversableF: Traverse[F]

  def F: Foldable[F] = TraversableF

  final override def traverse[G[_], A, B](fa: Free[F, A])(f: A => G[B])(implicit G: Applicative[G]): G[Free[F, B]] =
    fa.resume match {
      case Right(a)     => G.map(f(a))(Free.pure(_))
      case Left(ffreeA) => G.map(TraversableF.traverse(ffreeA)(traverse(_)(f)))(Free.roll(_))
    }

  // Override Traverse's map to use Free's map for better performance
  final override def map[A, B](fa: Free[F, A])(f: A => B): Free[F, B] = fa.map(f)
}

sealed abstract private[free] class FreeInstances extends FreeInstances1 {

  /**
   * `Free[S, ?]` has a monad for any type constructor `S[_]`.
   */
  implicit def catsFreeMonadForFree[S[_]]: Monad[Free[S, ?]] =
    new Monad[Free[S, ?]] with StackSafeMonad[Free[S, ?]] {
      def pure[A](a: A): Free[S, A] = Free.pure(a)
      override def map[A, B](fa: Free[S, A])(f: A => B): Free[S, B] = fa.map(f)
      def flatMap[A, B](a: Free[S, A])(f: A => Free[S, B]): Free[S, B] = a.flatMap(f)
    }

  implicit def catsFreeDeferForFree[S[_]]: Defer[Free[S, ?]] =
    new Defer[Free[S, ?]] {
      def defer[A](fa: => Free[S, A]): Free[S, A] =
        Free.defer(fa)
    }
}

sealed abstract private[free] class FreeInstances1 {

  implicit def catsFreeFoldableForFree[F[_]](
    implicit
    foldableF: Foldable[F]
  ): Foldable[Free[F, ?]] =
    new FreeFoldable[F] {
      val F = foldableF
    }

  implicit def catsFreeTraverseForFree[F[_]](
    implicit
    traversableF: Traverse[F]
  ): Traverse[Free[F, ?]] =
    new FreeTraverse[F] {
      val TraversableF = traversableF
    }
}
