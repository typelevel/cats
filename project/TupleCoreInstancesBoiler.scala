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
     * These special cases for N = 2 are needed because
     * of the deprecated versions in TupleInstances.
     * It will be removed once the deprecated ones are
     * deleted.
     */
    val showInst = if (arity == 2) "catsStdShowForNTuple2" else s"catsStdShowForTuple$arity"
    val bitraverseInst = if (arity == 2) "catsStdBitraverseForNTuple2" else s"catsStdBitraverseForTuple$arity"
    val stdInstances = if (arity == 2) "catsStdInstancesForNTuple2" else s"catsStdInstancesForTuple$arity"
    val stdCommMonadInst =
      if (arity == 2) "catsStdCommutativeMonadForNTuple2" else s"catsStdCommutativeMonadForTuple$arity"
    val stdCommFlatMapInst =
      if (arity == 2) "catsStdCommutativeFlatMapForNTuple2" else s"catsStdCommutativeFlatMapForTuple$arity"
    val stdMonadInst = if (arity == 2) "catsStdMonadForNTuple2" else s"catsStdMonadForTuple$arity"
    val stdFlatMapInst = if (arity == 2) "catsStdFlatMapForNTuple2" else s"catsStdFlatMapForTuple$arity"
    val flatMapTupleClass = if (arity == 2) "FlatMapNTuple2" else s"FlatMapTuple$arity"

    val `A..(N - 1)` = (0 until (arity - 1)).map(n => s"A$n")
    val `A..(N - 2)` = (0 until (arity - 2)).map(n => s"A$n")
    val `a..(n - 1)` = (0 until (arity - 1)).map(n => s"a$n")

    val `A0, A(N - 1)` = if (arity <= 1) "" else `A..(N - 1)`.mkString(", ")
    def `A0, A(N - 1)&`(a: String): String =
      if (arity <= 1) s"Tuple1[$a]" else `A..(N - 1)`.mkString("(", ", ", s", $a)")
    val `A0, A(N - 2)` = if (arity <= 2) "" else `A..(N - 2)`.mkString("", ", ", ", ")
    val `a0, a(n - 1)` = if (arity <= 1) "" else `a..(n - 1)`.mkString(", ")

    val `[A0, A(N - 1)]` = if (arity <= 1) "" else `A..(N - 1)`.mkString("[", ", ", "]")
    val `[A0, A(N - 2)]` = if (arity <= 2) "" else `A..(N - 2)`.mkString("[", ", ", "]")

    val `(A0, A(N - 1))` =
      if (arity == 1) "Tuple1[A0]"
      else if (arity == 2) "A0"
      else `A..(N - 1)`.mkString("(", ", ", ")")
    val `(A..N - 1, *)` =
      if (arity == 1) "Tuple1"
      else `A..(N - 1)`.mkString("(", ", ", ", *)")
    val `(A..N - 2, *, *)` =
      if (arity <= 2) "(*, *)"
      else `A..(N - 2)`.mkString("(", ", ", ", *, *)")

    def `fa._1..(n - 1) & `(a: String): String =
      if (arity <= 1) s"Tuple1($a)" else (0 until (arity - 1)).map(n => s"fa._${n + 1}").mkString("(", ", ", s", $a)")
    val `(fa._1..(n - 1))` =
      if (arity <= 1) "Tuple1.apply" else (0 until (arity - 1)).map(n => s"fa._${n + 1}").mkString("(", ", ", ", _)")
    val `pure(fa._1..(n - 2))` =
      if (arity <= 2) "" else (0 until (arity - 2)).map(n => s"G.pure(fa._${n + 1})").mkString("", ", ", ", ")

    def `constraints A..N`(c: String): String = synTypes.map(tpe => s"$tpe: $c[$tpe]").mkString("(implicit ", ", ", ")")
    def `constraints A..(N-1)`(c: String): String =
      if (arity <= 1) "" else `A..(N - 1)`.map(tpe => s"$tpe: $c[$tpe]").mkString("(implicit ", ", ", ")")
    def `parameters A..(N-1)`(c: String): String = `A..(N - 1)`.map(tpe => s"$tpe: $c[$tpe]").mkString(", ")
    def `combine A..(N - 1)`(a: String, b: String, last: String): String =
      if (arity <= 1) s"Tuple1($last)"
      else
        `A..(N - 1)`.zipWithIndex.iterator
          .map {
            case (an, i) => s"$an.combine($a._${i + 1}, $b._${i + 1})"
          }
          .mkString("(", ", ", s", $last)")

    val showMethod: String =
      synTypes.zipWithIndex.iterator
        .map {
          case (tpe, i) => s"$${$tpe.show(f._${i + 1})}"
        }
        .mkString("s\"(", ",", ")\"")

    val coflattenReturn =
      if (arity <= 1) "Tuple1[Tuple1[A]]"
      else
        s"${`A0, A(N - 1)&`(`A0, A(N - 1)&`("A"))}"

    val monadPureMethod: String =
      if (arity <= 1) "Tuple1(a)"
      else `A..(N - 1)`.map(n => s"$n.empty").mkString("(", ", ", ", a)")

    val tailRecMCombine =
      if (arity == 2) s"A0.combine(x, a0)"
      else
        `A..(N - 1)`.zipWithIndex.iterator
          .map {
            case (an, i) => s"$an.combine(x._${i + 1}, a$i)"
          }
          .mkString(", ")

    block"""
    |
    |package cats
    |package instances
    |
    |import cats.kernel.{CommutativeMonoid, CommutativeSemigroup}
    |import scala.annotation.tailrec
    |
    |private[instances] trait NTupleInstances extends NTupleInstances1 {
${if (arity > 1)
      block"""
-
-  implicit final def $bitraverseInst${`[A0, A(N - 2)]`}: Bitraverse[${`(A..N - 2, *, *)`}] =
-    new Bitraverse[${`(A..N - 2, *, *)`}] {
-      def bitraverse[G[_], A, B, C, D](fa: (${`A0, A(N - 2)`}A, B))(f: A => G[C], g: B => G[D])(implicit G: Applicative[G]): G[(${`A0, A(N - 2)`}C, D)] =
-        G.tuple$arity(${`pure(fa._1..(n - 2))`}f(fa._${arity - 1}), g(fa._$arity))
-      def bifoldLeft[A, B, C](fa: (${`A0, A(N - 2)`}A, B), c: C)(f: (C, A) => C, g: (C, B) => C): C =
-        g(f(c, fa._${arity - 1}), fa._$arity)
-      def bifoldRight[A, B, C](fa: (${`A0, A(N - 2)`}A, B), c: Eval[C])(f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C]): Eval[C] =
-        g(fa._$arity, f(fa._${arity - 1}, c))
-    }
"""
    else block""}
-  implicit final def $showInst[${`A..N`}]${`constraints A..N`("Show")}: Show[${`(A..N)`}] =
-    new Show[${`(A..N)`}] {
-      def show(f: ${`(A..N)`}): String = $showMethod
-    }
-  implicit final def $stdInstances${`[A0, A(N - 1)]`}: Traverse[${`(A..N - 1, *)`}] with Comonad[${`(A..N - 1, *)`}] with Reducible[${`(A..N - 1, *)`}] =
-    new Traverse[${`(A..N - 1, *)`}] with Comonad[${`(A..N - 1, *)`}] with Reducible[${`(A..N - 1, *)`}] {
-      def traverse[G[_], A, B](fa: ${`A0, A(N - 1)&`("A")})(f: A => G[B])(implicit G: Applicative[G]): G[${`A0, A(N - 1)&`(
      "B"
    )}] =
-        G.map(f(fa._$arity))(${`(fa._1..(n - 1))`})
-      def foldLeft[A, B](fa: ${`A0, A(N - 1)&`("A")}, b: B)(f: (B, A) => B): B = f(b, fa._$arity)
-      def foldRight[A, B](fa: ${`A0, A(N - 1)&`("A")}, lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = f(fa._$arity, lb)
-      override def map[A, B](fa: ${`A0, A(N - 1)&`("A")})(f: A => B): ${`A0, A(N - 1)&`("B")} =
-        ${`fa._1..(n - 1) & `(s"f(fa._$arity)")}
-      def coflatMap[A, B](fa: ${`A0, A(N - 1)&`("A")})(f: (${`A0, A(N - 1)&`("A")}) => B): ${`A0, A(N - 1)&`("B")} =
-        ${`fa._1..(n - 1) & `("f(fa)")}
-      def extract[A](fa: ${`A0, A(N - 1)&`("A")}): A = fa._$arity
-      override def coflatten[A](fa: ${`A0, A(N - 1)&`("A")}): $coflattenReturn = ${`fa._1..(n - 1) & `("fa")}
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
    |}
    |private[instances] sealed trait NTupleInstances1 extends NTupleInstances2 {
-  implicit final def $stdCommMonadInst${`[A0, A(N - 1)]`}${`constraints A..(N-1)`("CommutativeMonoid")}: CommutativeMonad[${`(A..N - 1, *)`}] =
-    new FlatMapTuple$arity${`[A0, A(N - 1)]`}(${`A0, A(N - 1)`}) with CommutativeMonad[${`(A..N - 1, *)`}] {
-      def pure[A](a: A): ${`A0, A(N - 1)&`("A")} = $monadPureMethod
-    }
    |}
    |private[instances] sealed trait NTupleInstances2 extends NTupleInstances3 {
-  implicit final def $stdCommFlatMapInst${`[A0, A(N - 1)]`}${`constraints A..(N-1)`("CommutativeSemigroup")}: CommutativeFlatMap[${`(A..N - 1, *)`}] =
-    new FlatMapTuple$arity${`[A0, A(N - 1)]`}(${`A0, A(N - 1)`}) with CommutativeFlatMap[${`(A..N - 1, *)`}]
    |}
    |private[instances] sealed trait NTupleInstances3 extends NTupleInstances4 {
-  implicit def $stdMonadInst${`[A0, A(N - 1)]`}${`constraints A..(N-1)`("Monoid")}: Monad[${`(A..N - 1, *)`}] =
-    new FlatMapTuple$arity${`[A0, A(N - 1)]`}(${`A0, A(N - 1)`}) with Monad[${`(A..N - 1, *)`}] {
-      def pure[A](a: A): ${`A0, A(N - 1)&`("A")} = $monadPureMethod
-    }
    |}
    |private[instances] sealed trait NTupleInstances4 {
-  implicit def $stdFlatMapInst${`[A0, A(N - 1)]`}${`constraints A..(N-1)`("Semigroup")}: FlatMap[${`(A..N - 1, *)`}] =
-    new FlatMapTuple$arity${`[A0, A(N - 1)]`}(${`A0, A(N - 1)`})
    |}
-
-private[instances] class $flatMapTupleClass${`[A0, A(N - 1)]`}(${`parameters A..(N-1)`("Semigroup")}) extends FlatMap[${`(A..N - 1, *)`}] {
-  override def ap[A, B](ff: ${`A0, A(N - 1)&`("A => B")})(fa: ${`A0, A(N - 1)&`("A")}): ${`A0, A(N - 1)&`("B")} =
-    ${`combine A..(N - 1)`("ff", "fa", s"ff._$arity(fa._$arity)")}
-  override def product[A, B](fa: ${`A0, A(N - 1)&`("A")}, fb: ${`A0, A(N - 1)&`("B")}): ${`A0, A(N - 1)&`("(A, B)")} =
-    ${`combine A..(N - 1)`("fa", "fb", s"(fa._$arity, fb._$arity)")}
-  override def map[A, B](fa: ${`A0, A(N - 1)&`("A")})(f: A => B): ${`A0, A(N - 1)&`("B")} =
-    ${`fa._1..(n - 1) & `(s"f(fa._$arity)")}
-  def flatMap[A, B](fa: ${`A0, A(N - 1)&`("A")})(f: A => ${`A0, A(N - 1)&`("B")}): ${`A0, A(N - 1)&`("B")} = {
     ${if (arity > 1) block"""
     -    val xb = f(fa._$arity)
     -    ${`combine A..(N - 1)`("fa", "xb", s"xb._$arity")}
     """
    else block"""
     -    f(fa._1)
     """}
-  }
-  override def productR[A, B](a: ${`A0, A(N - 1)&`("A")})(b: ${`A0, A(N - 1)&`("B")}): ${`A0, A(N - 1)&`("B")} =
-    ${`combine A..(N - 1)`("a", "b", s"b._$arity")}
-  override def productL[A, B](a: ${`A0, A(N - 1)&`("A")})(b: ${`A0, A(N - 1)&`("B")}): ${`A0, A(N - 1)&`("A")} =
-    ${`combine A..(N - 1)`("a", "b", s"a._$arity")}
-  override def mproduct[A, B](fa: ${`A0, A(N - 1)&`("A")})(f: A => ${`A0, A(N - 1)&`("B")}): ${`A0, A(N - 1)&`(
      "(A, B)"
    )} = {
-    val xb = f(fa._$arity)
-    ${`combine A..(N - 1)`("fa", "xb", s"(fa._$arity, xb._$arity)")}
-  }
-  def tailRecM[A, B](a: A)(f: A => ${`A0, A(N - 1)&`("Either[A, B]")}): ${`A0, A(N - 1)&`("B")} = {
-    @tailrec
${if (arity > 1) block"""
-    def loop(x: ${`(A0, A(N - 1))`}, aa: A): ${`A0, A(N - 1)&`("B")} =
-      f(aa) match {
-        case (${`a0, a(n - 1)`}, Right(b))    => ($tailRecMCombine, b)
-        case (${`a0, a(n - 1)`}, Left(nextA)) => loop(($tailRecMCombine), nextA)
-      }
-    f(a) match {
-      case (${`a0, a(n - 1)`}, Right(b))    => (${`a0, a(n - 1)`}, b)
-      case (${`a0, a(n - 1)`}, Left(nextA)) => loop((${`a0, a(n - 1)`}), nextA)
"""
    else block"""
-    def loop(aa: A): Tuple1[B] =
-      f(aa) match {
-        case Tuple1(Right(b))    => Tuple1(b)
-        case Tuple1(Left(nextA)) => loop(nextA)
-      }
-    f(a) match {
-      case Tuple1(Right(b))    => Tuple1(b)
-      case Tuple1(Left(nextA)) => loop(nextA)
"""}
-    }
-  }
-}"""
  }
}
