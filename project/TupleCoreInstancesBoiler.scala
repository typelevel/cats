import sbt._

import Boilerplate._
import Boilerplate.{Template, TemplateVals}
import sbt.File

object GenTupleCoreInstances extends Template {
  def filename(root: sbt.File): File =
    root / "cats" / "instances" / "NTupleInstances.scala"

  def content(tv: TemplateVals): String = {
    import tv._

    /**
     * These special cases for N = 2 is needed because of the
     * deprecated `catsStdShowForTuple2` in TupleInstances.
     * It will be removed once deprecated one is deleted.
     */
    val showInst       = if(arity == 2) "catsStdShowForNTuple2" else s"catsStdShowForTuple$arity"
    val bitraverseInst = if(arity == 2) "catsStdBitraverseForNTuple2" else s"catsStdBitraverseForTuple$arity"
    val stdInstances   = if(arity == 2) "catsStdInstancesForNTuple2" else s"catsStdInstancesForTuple$arity"

    def constraints(name: String): String =
      synTypes.map(tpe => s"$tpe: $name[$tpe]").mkString(", ")

    val showMethod: String =
      synTypes.zipWithIndex.iterator
        .map {
          case (tpe, i) => s"$${$tpe.show(f._${i + 1})}"
        }
        .mkString("s\"(", ",", ")\"")

    val `A..(N - 1)` = (0 until (arity - 1)).map(n => s"A$n")
    val `A..(N - 2)` = (0 until (arity - 2)).map(n => s"A$n")

    def `A0, A(N - 1)`(a: String): String = if (arity <= 1) s"Tuple1[$a]" else `A..(N - 1)`.mkString("(", ", ", s", $a)")
    val `A0, A(N - 2)` = if (arity <= 2) "" else `A..(N - 2)`.mkString("", ", ", ", ")

    val `[A0, A(N - 1)]` = if (arity <= 1) "" else `A..(N - 1)`.mkString("[", ", ", "]")
    val `[A0, A(N - 2)]` = if (arity <= 2) "" else `A..(N - 2)`.mkString("[", ", ", "]")
    val `(A..N - 1, *)` =
      if (arity == 1) "Tuple1"
      else `A..(N - 1)`.mkString("(", ", ", ", *)")
    val `(A..N - 2, *, *)` =
      if (arity <= 2) "(*, *)"
      else `A..(N - 2)`.mkString("(", ", ", ", *, *)")

    val `pure(t._n)` = if (arity <= 2) "" else (0 until (arity - 2)).map(n => s"G.pure(t._${n + 1})").mkString("", ", ", ", ")
    def `t._n`(a: String): String = if (arity <= 1) s"Tuple1($a)" else (0 until (arity - 1)).map(n => s"t._${n + 1}").mkString("(", ", ", s", $a)")
    val `(t._n)` = if (arity <= 1) "Tuple1.apply" else (0 until (arity - 1)).map(n => s"t._${n + 1}").mkString("(", ", ", ", _)")

    val coflattenReturn = if (arity <= 1) "Tuple1[Tuple1[A]]" else s"${`A0, A(N - 1)`({`A0, A(N - 1)`("A")})}"

    block"""
      |
      |package cats
      |package instances
      |
      |private[instances] trait NTupleInstances {
      ${
      if(arity > 1)
        block"""
      -  implicit def $bitraverseInst${`[A0, A(N - 2)]`}: Bitraverse[${`(A..N - 2, *, *)`}] =
      -    new Bitraverse[${`(A..N - 2, *, *)`}] {
      -      def bitraverse[G[_], A, B, C, D](t: (${`A0, A(N - 2)`}A, B))(f: A => G[C], g: B => G[D])(implicit G: Applicative[G]): G[(${`A0, A(N - 2)`}C, D)] =
      -        G.tuple$arity(${`pure(t._n)`}f(t._${arity - 1}), g(t._$arity))
      -
      -      def bifoldLeft[A, B, C](t: (${`A0, A(N - 2)`}A, B), c: C)(f: (C, A) => C, g: (C, B) => C): C =
      -        g(f(c, t._${arity - 1}), t._$arity)
      -
      -      def bifoldRight[A, B, C](t: (${`A0, A(N - 2)`}A, B), c: Eval[C])(f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C]): Eval[C] =
      -        g(t._$arity, f(t._${arity - 1}, c))
      -    }
          """
      else block""
    }
      -  implicit final def $showInst[${`A..N`}](implicit ${constraints("Show")}): Show[${`(A..N)`}] =
      -    new Show[${`(A..N)`}] {
      -      def show(f: ${`(A..N)`}): String = $showMethod
      -    }
      -  implicit def $stdInstances${`[A0, A(N - 1)]`}: Traverse[${`(A..N - 1, *)`}] with Comonad[${`(A..N - 1, *)`}] with Reducible[${`(A..N - 1, *)`}] =
      -    new Traverse[${`(A..N - 1, *)`}] with Comonad[${`(A..N - 1, *)`}] with Reducible[${`(A..N - 1, *)`}] {
      -      def traverse[G[_], A, B](t: ${`A0, A(N - 1)`("A")})(f: A => G[B])(implicit G: Applicative[G]): G[${`A0, A(N - 1)`("B")}] =
      -        G.map(f(t._$arity))(${`(t._n)`})
      -
      -      def foldLeft[A, B](t: ${`A0, A(N - 1)`("A")}, b: B)(f: (B, A) => B): B = f(b, t._$arity)
      -
      -      def foldRight[A, B](t: ${`A0, A(N - 1)`("A")}, lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = f(t._$arity, lb)
      -
      -      override def map[A, B](t: ${`A0, A(N - 1)`("A")})(f: A => B): ${`A0, A(N - 1)`("B")} = ${`t._n`(s"f(t._$arity)")}
      -
      -      def coflatMap[A, B](t: ${`A0, A(N - 1)`("A")})(f: (${`A0, A(N - 1)`("A")}) => B): ${`A0, A(N - 1)`("B")} = ${`t._n`("f(t)")}
      -
      -      def extract[A](t: ${`A0, A(N - 1)`("A")}): A = t._$arity
      -
      -      override def coflatten[A](t: ${`A0, A(N - 1)`("A")}): $coflattenReturn = ${`t._n`("t")}
      -
      -      override def foldMap[A, B](t: ${`A0, A(N - 1)`("A")})(f: A => B)(implicit B: Monoid[B]): B = f(t._$arity)
      -
      -      override def reduce[A](t: ${`A0, A(N - 1)`("A")})(implicit A: Semigroup[A]): A = t._$arity
      -
      -      def reduceLeftTo[A, B](t: ${`A0, A(N - 1)`("A")})(f: A => B)(g: (B, A) => B): B = g(f(t._$arity), t._$arity)
      -
      -      override def reduceLeft[A](t: ${`A0, A(N - 1)`("A")})(f: (A, A) => A): A = f(t._$arity, t._$arity)
      -
      -      override def reduceLeftToOption[A, B](t: ${`A0, A(N - 1)`("A")})(f: A => B)(g: (B, A) => B): Option[B] =
      -        Some(g(f(t._$arity), t._$arity))
      -
      -      override def reduceRight[A](t: ${`A0, A(N - 1)`("A")})(f: (A, Eval[A]) => Eval[A]): Eval[A] =
      -        f(t._$arity, Now(t._$arity))
      -
      -      def reduceRightTo[A, B](t: ${`A0, A(N - 1)`("A")})(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B] =
      -        g(t._$arity, Now(f(t._$arity)))
      -
      -      override def reduceRightToOption[A, B](t: ${`A0, A(N - 1)`("A")})(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[Option[B]] =
      -        Now(Some(f(t._$arity)))
      -
      -      override def reduceMap[A, B](t: ${`A0, A(N - 1)`("A")})(f: A => B)(implicit B: Semigroup[B]): B =
      -        f(t._$arity)
      -
      -      override def size[A](t: ${`A0, A(N - 1)`("A")}): Long = 1L
      -
      -      override def get[A](t: ${`A0, A(N - 1)`("A")})(idx: Long): Option[A] =
      -        if (idx == 0L) Some(t._$arity) else None
      -
      -      override def exists[A](t: ${`A0, A(N - 1)`("A")})(p: A => Boolean): Boolean = p(t._$arity)
      -
      -      override def forall[A](t: ${`A0, A(N - 1)`("A")})(p: A => Boolean): Boolean = p(t._$arity)
      -
      -      override def isEmpty[A](t: ${`A0, A(N - 1)`("A")}): Boolean = false
      -    }
      -
      |}"""
  }
}
