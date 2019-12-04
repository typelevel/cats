package cats
package free

import cats.arrow.FunctionK
import cats.data.Const

/**
 * Invariant Monoidal for Free
 */
sealed abstract class FreeInvariantMonoidal[F[_], A] extends Product with Serializable { self =>
  import FreeInvariantMonoidal.{lift, FA, Imap, Zip}

  def imap[B](f: A => B)(g: B => A): FA[F, B] =
    Imap(this, f, g)

  def product[B](fb: FA[F, B]): FA[F, (A, B)] =
    Zip(this, fb)

  /** Interprets/Runs the sequence of operations using the semantics of `InvariantMonoidal[G]` */
  def foldMap[G[_]](nt: FunctionK[F, G])(implicit im: InvariantMonoidal[G]): G[A]
  // Note that implementing a concrete `foldMap` here does not work because
  // `Zip extends G[(A, B)]` confuses the type inferance when pattern matching on `this`.

  /** Interpret/run the operations using the semantics of `InvariantMonoidal[F]`. */
  final def fold(implicit F: InvariantMonoidal[F]): F[A] =
    foldMap(FunctionK.id[F])

  /** Interpret this algebra into another InvariantMonoidal */
  final def compile[G[_]](f: FunctionK[F, G]): FA[G, A] =
    foldMap[FA[G, *]] {
      new FunctionK[F, FA[G, *]] { def apply[B](fb: F[B]): FA[G, B] = lift(f(fb)) }
    }

  /** Interpret this algebra into a Monoid */
  final def analyze[M: Monoid](f: FunctionK[F, λ[α => M]]): M =
    foldMap[Const[M, *]](
      new FunctionK[F, Const[M, *]] { def apply[B](fb: F[B]): Const[M, B] = Const(f(fb)) }
    ).getConst
}

object FreeInvariantMonoidal {
  type FA[F[_], A] = FreeInvariantMonoidal[F, A]

  final private case class Pure[F[_], A](a: A) extends FA[F, A] {
    def foldMap[G[_]](nt: FunctionK[F, G])(implicit im: InvariantMonoidal[G]): G[A] =
      im.point(a)
  }

  final private case class Suspend[F[_], A](fa: F[A]) extends FA[F, A] {
    def foldMap[G[_]](nt: FunctionK[F, G])(implicit im: InvariantMonoidal[G]): G[A] =
      nt(fa)
  }

  final private case class Zip[F[_], A, B](fa: FA[F, A], fb: FA[F, B]) extends FA[F, (A, B)] {
    def foldMap[G[_]](nt: FunctionK[F, G])(implicit im: InvariantMonoidal[G]): G[(A, B)] =
      im.product(fa.foldMap(nt), fb.foldMap(nt))
  }

  final private case class Imap[F[_], A, B](fa: FA[F, A], f: A => B, g: B => A) extends FA[F, B] {
    def foldMap[G[_]](nt: FunctionK[F, G])(implicit im: InvariantMonoidal[G]): G[B] =
      im.imap(fa.foldMap(nt))(f)(g)
  }

  def pure[F[_], A](a: A): FA[F, A] =
    Pure(a)

  def lift[F[_], A](fa: F[A]): FA[F, A] =
    Suspend(fa)

  /** `FreeInvariantMonoidal[S, *]` has a FreeInvariantMonoidal for any type constructor `S[_]`. */
  implicit def catsFreeInvariantMonoidal[S[_]]: InvariantMonoidal[FA[S, *]] =
    new InvariantMonoidal[FA[S, *]] {
      def unit: FA[S, Unit] = FreeInvariantMonoidal.pure(())
      def imap[A, B](fa: FA[S, A])(f: A => B)(g: B => A): FA[S, B] = fa.imap(f)(g)
      def product[A, B](fa: FA[S, A], fb: FA[S, B]): FA[S, (A, B)] = fa.product(fb)
    }
}
