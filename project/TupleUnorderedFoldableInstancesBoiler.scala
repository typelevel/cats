import sbt._

import Boilerplate._
import Boilerplate.{Template, TemplateVals}
import sbt.File

object GenTupleUnorderedFoldableInstances extends Template {
  def filename(root: sbt.File): File =
    root / "cats" / "instances" / "NTupleUnorderedFoldableInstances.scala"

  def content(tv: TemplateVals): String = {
    import tv._

    block"""
    |
    |package cats
    |package instances
    |
    |private[cats] trait NTupleUnorderedFoldableInstances {
-  implicit final def catsUnorderedFoldableInstancesForTuple$arity${`[A0, A(N - 1)]`}: Traverse[${`(A..N - 1, *)`}] with Reducible[${`(A..N - 1, *)`}] =
-    new Traverse[${`(A..N - 1, *)`}] with Reducible[${`(A..N - 1, *)`}] {
-      def traverse[G[_], A, B](fa: ${`A0, A(N - 1)&`(
      "A"
    )})(f: A => G[B])(implicit G: Applicative[G]): G[${`A0, A(N - 1)&`(
      "B"
    )}] =
-        G.map(f(fa._$arity))(${`(fa._1..(n - 1))`})
-      def foldLeft[A, B](fa: ${`A0, A(N - 1)&`("A")}, b: B)(f: (B, A) => B): B = f(b, fa._$arity)
-      def foldRight[A, B](fa: ${`A0, A(N - 1)&`("A")}, lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = f(fa._$arity, lb)
-      override def map[A, B](fa: ${`A0, A(N - 1)&`("A")})(f: A => B): ${`A0, A(N - 1)&`("B")} =
-        ${`fa._1..(n - 1) & `(s"f(fa._$arity)")}
-      override def foldMap[A, B](fa: ${`A0, A(N - 1)&`("A")})(f: A => B)(implicit B: Monoid[B]): B = f(fa._$arity)
-      override def reduce[A](fa: ${`A0, A(N - 1)&`("A")})(implicit A: Semigroup[A]): A = fa._$arity
-      def reduceLeftTo[A, B](fa: ${`A0, A(N - 1)&`("A")})(f: A => B)(g: (B, A) => B): B = f(fa._$arity)
-      override def reduceLeft[A](fa: ${`A0, A(N - 1)&`("A")})(f: (A, A) => A): A = fa._$arity
-      override def reduceLeftToOption[A, B](fa: ${`A0, A(N - 1)&`("A")})(f: A => B)(g: (B, A) => B): Option[B] = Some(f(fa._$arity))
-      override def reduceRight[A](fa: ${`A0, A(N - 1)&`("A")})(f: (A, Eval[A]) => Eval[A]): Eval[A] = Now(fa._$arity)
-      def reduceRightTo[A, B](fa: ${`A0, A(N - 1)&`("A")})(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B] = Now(f(fa._$arity))
-      override def reduceRightToOption[A, B](fa: ${`A0, A(N - 1)&`("A")})(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[Option[B]] = Now(Some(f(fa._$arity)))
-      override def reduceMap[A, B](fa: ${`A0, A(N - 1)&`("A")})(f: A => B)(implicit B: Semigroup[B]): B = f(fa._$arity)
-      override def size[A](fa: ${`A0, A(N - 1)&`("A")}): Long = 1L
-      override def get[A](fa: ${`A0, A(N - 1)&`("A")})(idx: Long): Option[A] = if (idx == 0L) Some(fa._$arity) else None
-      override def exists[A](fa: ${`A0, A(N - 1)&`("A")})(p: A => Boolean): Boolean = p(fa._$arity)
-      override def forall[A](fa: ${`A0, A(N - 1)&`("A")})(p: A => Boolean): Boolean = p(fa._$arity)
-      override def isEmpty[A](fa: ${`A0, A(N - 1)&`("A")}): Boolean = false
-    }
    |}"""
  }
}
