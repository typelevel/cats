import sbt.*

import Boilerplate.*
import Boilerplate.{Template, TemplateVals}
import sbt.File

object GenTupleUnorderedFoldableInstances extends Template {
  override def range = 1 to 11
  override def filename(root: sbt.File): File =
    root / "cats" / "instances" / "NTupleUnorderedFoldableInstances.scala"

  override def content(tv: TemplateVals): String = {
    import tv.*

    block"""
    |
    |package cats
    |package instances
    |
    |private[cats] trait NTupleUnorderedFoldableInstances {
    |  protected type γ[_]
    |
    |  private def instance[F[_] <: Product](
    |    trav: (F[Any], Applicative[γ], Any => γ[Any]) => γ[F[Any]]
    |  ): Traverse[F] with Reducible[F] =
    |    new Traverse[F] with Reducible[F] {
    |      def traverse[G[_], A, B](fa: F[A])(f: A => G[B])(implicit G: Applicative[G]) =
    |        trav(fa.asInstanceOf[F[Any]], G.asInstanceOf[Applicative[γ]], f.asInstanceOf[Any => γ[Any]]).asInstanceOf[G[F[B]]]
    |      @inline private def last[A](fa: F[A]): A =
    |        fa.productElement(fa.productArity - 1).asInstanceOf[A]
    |      def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B = f(b, last(fa))
    |      def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = f(last(fa), lb)
    |      override def foldMap[A, B](fa: F[A])(f: A => B)(implicit B: Monoid[B]): B = f(last(fa))
    |      override def reduce[A](fa: F[A])(implicit A: Semigroup[A]): A = last(fa)
    |      def reduceLeftTo[A, B](fa: F[A])(f: A => B)(g: (B, A) => B): B = f(last(fa))
    |      override def reduceLeft[A](fa: F[A])(f: (A, A) => A): A = last(fa)
    |      override def reduceLeftToOption[A, B](fa: F[A])(f: A => B)(g: (B, A) => B): Option[B] = Some(f(last(fa)))
    |      override def reduceRight[A](fa: F[A])(f: (A, Eval[A]) => Eval[A]): Eval[A] = Now(last(fa))
    |      def reduceRightTo[A, B](fa: F[A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B] = Now(f(last(fa)))
    |      override def reduceRightToOption[A, B](fa: F[A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[Option[B]] = Now(Some(f(last(fa))))
    |      override def reduceMap[A, B](fa: F[A])(f: A => B)(implicit B: Semigroup[B]): B = f(last(fa))
    |      override def size[A](fa: F[A]): Long = 1L
    |      override def get[A](fa: F[A])(idx: Long): Option[A] = if (idx == 0L) Some(last(fa)) else None
    |      override def exists[A](fa: F[A])(p: A => Boolean): Boolean = p(last(fa))
    |      override def forall[A](fa: F[A])(p: A => Boolean): Boolean = p(last(fa))
    |      override def isEmpty[A](fa: F[A]): Boolean = false
    |    }
    -
    -  implicit final def catsUnorderedFoldableInstancesForTuple$arity${`[A0, A(N - 1)]`}
    -    : Traverse[${`(A..N - 1, *)`}] with Reducible[${`(A..N - 1, *)`}]
    -    = instance((fa, G, f) => G.map(f(fa._$arity))(x => fa.copy(_$arity = x)))
    |}"""
  }
}
