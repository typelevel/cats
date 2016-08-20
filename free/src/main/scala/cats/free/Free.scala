package cats
package free

import scala.annotation.tailrec

import cats.arrow.FunctionK
import cats.data.Xor

/**
 * A free operational monad for some functor `S`. Binding is done
 * using the heap instead of the stack, allowing tail-call
 * elimination.
 */
sealed abstract class Free[S[_], A] extends Product with Serializable {

  import Free.{ Pure, Suspend, FlatMapped }

  final def map[B](f: A => B): Free[S, B] =
    flatMap(a => Pure(f(a)))

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
    case FlatMapped(Pure(a), f) => f(a).step
    case x => x
  }

  /**
   * Evaluate a single layer of the free monad.
   */
  @tailrec
  final def resume(implicit S: Functor[S]): Xor[S[Free[S, A]], A] = this match {
    case Pure(a) => Xor.Right(a)
    case Suspend(t) => Xor.Left(S.map(t)(Pure(_)))
    case FlatMapped(c, f) =>
      c match {
        case Pure(a) => f(a).resume
        case Suspend(t) => Xor.Left(S.map(t)(f))
        case FlatMapped(d, g) => d.flatMap(dd => g(dd).flatMap(f)).resume
      }
  }

  /**
   * Run to completion, using a function that extracts the resumption
   * from its suspension functor.
   */
  final def go(f: S[Free[S, A]] => Free[S, A])(implicit S: Functor[S]): A = {
    @tailrec def loop(t: Free[S, A]): A =
      t.resume match {
        case Xor.Left(s) => loop(f(s))
        case Xor.Right(r) => r
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
  final def runM[M[_]](f: S[Free[S, A]] => M[Free[S, A]])(implicit S: Functor[S], M: Monad[M], R: RecursiveTailRecM[M]): M[A] = {
    def step(t: S[Free[S, A]]): M[Either[S[Free[S, A]], A]] =
      M.map(f(t))(_.resume.toEither)

    resume match {
      case Xor.Left(s) => R.sameType(M).tailRecM(s)(step)
      case Xor.Right(r) => M.pure(r)
    }
  }

  /**
   * Run to completion, using monadic recursion to evaluate the
   * resumption in the context of `S`.
   */
  final def runTailRec(implicit S: Monad[S], r: RecursiveTailRecM[S]): S[A] = {
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
    r.sameType(S).tailRecM(this)(step)
  }
  /**
   * Run to completion, using monadic recursion to evaluate the
   * resumption in the context of `S` without a guarantee of stack-safety
   */
  final def runTailRecUnsafe(implicit S: Monad[S]): S[A] =
    runTailRec(S, RecursiveTailRecM.create)

  /**
   * Catamorphism for `Free`.
   *
   * Run to completion, mapping the suspension with the given
   * transformation at each step and accumulating into the monad `M`.
   *
   * This method uses `tailRecM` to provide stack-safety.
   */
  final def foldMap[M[_]](f: FunctionK[S, M])(implicit M: Monad[M], r: RecursiveTailRecM[M]): M[A] =
    r.sameType(M).tailRecM(this)(_.step match {
      case Pure(a) => M.pure(Right(a))
      case Suspend(sa) => M.map(f(sa))(Right(_))
      case FlatMapped(c, g) => M.map(c.foldMap(f))(cc => Left(g(cc)))
    })

  /**
   * Same as foldMap but without a guarantee of stack safety. If the recursion is shallow
   * enough, this will work
   */
  final def foldMapUnsafe[M[_]](f: FunctionK[S, M])(implicit M: Monad[M]): M[A] =
    foldMap[M](f)(M, RecursiveTailRecM.create)


  /**
   * Compile your free monad into another language by changing the
   * suspension functor using the given natural transformation `f`.
   *
   * If your natural transformation is effectful, be careful. These
   * effects will be applied by `compile`.
   */
  final def compile[T[_]](f: FunctionK[S, T]): Free[T, A] =
    foldMapUnsafe[Free[T, ?]] { // this is safe because Free is stack safe
      new FunctionK[S, Free[T, ?]] {
        def apply[B](fa: S[B]): Free[T, B] = Suspend(f(fa))
      }
    }(Free.catsFreeMonadForFree)

  override def toString(): String =
    "Free(...)"
}

object Free {

  /**
   * Return from the computation with the given value.
   */
  private[free] final case class Pure[S[_], A](a: A) extends Free[S, A]

  /** Suspend the computation with the given suspension. */
  private[free] final case class Suspend[S[_], A](a: S[A]) extends Free[S, A]

  /** Call a subroutine and continue with the given function. */
  private[free] final case class FlatMapped[S[_], B, C](c: Free[S, C], f: C => Free[S, B]) extends Free[S, B]

  /**
   * Lift a pure `A` value into the free monad.
   */
  def pure[S[_], A](a: A): Free[S, A] = Pure(a)

  /**
   * Lift an `F[A]` value into the free monad.
   */
  def liftF[F[_], A](value: F[A]): Free[F, A] = Suspend(value)

  /**
   * Suspend the creation of a `Free[F, A]` value.
   */
  def suspend[F[_], A](value: => Free[F, A]): Free[F, A] =
    pure(()).flatMap(_ => value)

  /**
   * This method is used to defer the application of an Inject[F, G]
   * instance. The actual work happens in
   * `FreeInjectPartiallyApplied#apply`.
   *
   * This method exists to allow the `F` and `G` parameters to be
   * bound independently of the `A` parameter below.
   */
  def inject[F[_], G[_]]: FreeInjectPartiallyApplied[F, G] =
    new FreeInjectPartiallyApplied

  /**
   * Pre-application of an injection to a `F[A]` value.
   */
  final class FreeInjectPartiallyApplied[F[_], G[_]] private[free] {
    def apply[A](fa: F[A])(implicit I: Inject[F, G]): Free[G, A] =
      Free.liftF(I.inj(fa))
  }

  /**
   * `Free[S, ?]` has a monad for any type constructor `S[_]`.
   */
  implicit def catsFreeMonadForFree[S[_]]: Monad[Free[S, ?]] with RecursiveTailRecM[Free[S, ?]] =
    new Monad[Free[S, ?]] with RecursiveTailRecM[Free[S, ?]] {
      def pure[A](a: A): Free[S, A] = Free.pure(a)
      override def map[A, B](fa: Free[S, A])(f: A => B): Free[S, B] = fa.map(f)
      def flatMap[A, B](a: Free[S, A])(f: A => Free[S, B]): Free[S, B] = a.flatMap(f)
      def tailRecM[A, B](a: A)(f: A => Free[S, Either[A, B]]): Free[S, B] =
        f(a).flatMap(_ match {
          case Left(a1) => tailRecM(a1)(f) // recursion OK here, since Free is lazy
          case Right(b) => pure(b)
        })
    }

  /**
   * Perform a stack-safe monadic fold from the source context `F`
   * into the target monad `G`.
   *
   * This method can express short-circuiting semantics. Even when
   * `fa` is an infinite structure, this method can potentially
   * terminate if the `foldRight` implementation for `F` and the
   * `tailRecM` implementation for `G` are sufficiently lazy.
   */
  def foldLeftM[F[_]: Foldable, G[_]: Monad: RecursiveTailRecM, A, B](fa: F[A], z: B)(f: (B, A) => G[B]): G[B] =
    unsafeFoldLeftM[F, Free[G, ?], A, B](fa, z) { (b, a) =>
      Free.liftF(f(b, a))
    }.runTailRec

  private def unsafeFoldLeftM[F[_], G[_], A, B](fa: F[A], z: B)(f: (B, A) => G[B])(implicit F: Foldable[F], G: Monad[G]): G[B] =
    F.foldRight(fa, Always((w: B) => G.pure(w))) { (a, lb) =>
      Always((w: B) => G.flatMap(f(w, a))(lb.value))
    }.value.apply(z)
}
