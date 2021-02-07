import sbt._

import Boilerplate._
import Boilerplate.{Template, TemplateVals}
import sbt.File

object GenTupleMonadInstances extends Template {
  def filename(root: sbt.File): File =
    root / "cats" / "instances" / "NTupleMonadInstances.scala"

  def content(tv: TemplateVals): String = {
    import tv._

    /**
     * This special case for N = 2 is needed because
     * of the deprecated versions in TupleInstances.
     * It will be removed once the deprecated ones are
     * deleted.
     */
    val flatMapTupleClass = if (arity == 2) "FlatMapNTuple2" else s"FlatMapTuple$arity"

    def `combine A..(N - 1)`(a: String, b: String, last: String): String =
      if (arity <= 1) s"Tuple1($last)"
      else
        `A..(N - 1)`.zipWithIndex.iterator
          .map { case (an, i) =>
            s"$an.combine($a._${i + 1}, $b._${i + 1})"
          }
          .mkString("(", ", ", s", $last)")

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
          .map { case (an, i) =>
            s"$an.combine(x._${i + 1}, a$i)"
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
    |private[cats] trait NTupleMonadInstances extends NTupleMonadInstances1 {
    -  implicit final def catsStdInstancesForTuple$arity${`[A0, A(N - 1)]`}: Comonad[${`(A..N - 1, *)`}] =
    -    new Comonad[${`(A..N - 1, *)`}] {
    -      def coflatMap[A, B](fa: ${`A0, A(N - 1)&`("A")})(f: (${`A0, A(N - 1)&`("A")}) => B): ${`A0, A(N - 1)&`(
      "B"
    )} = ${`fa._1..(n - 1) & `(
      "f(fa)"
    )}
    -      def extract[A](fa: ${`A0, A(N - 1)&`("A")}): A = fa._$arity
    -      override def map[A, B](fa: ${`A0, A(N - 1)&`("A")})(f: A => B): ${`A0, A(N - 1)&`("B")} =
    -        ${`fa._1..(n - 1) & `(s"f(fa._$arity)")}
    -      override def coflatten[A](fa: ${`A0, A(N - 1)&`("A")}): $coflattenReturn = ${`fa._1..(n - 1) & `("fa")}
    -    }
    |}
    |private[cats] sealed trait NTupleMonadInstances1 extends NTupleMonadInstances2 {
    -  implicit final def catsStdCommutativeMonadForTuple$arity${`[A0, A(N - 1)]`}${`constraints A..(N-1)`(
      "CommutativeMonoid"
    )}: CommutativeMonad[${`(A..N - 1, *)`}] =
    -    new FlatMapTuple$arity${`[A0, A(N - 1)]`}(${`A0, A(N - 1)`}) with CommutativeMonad[${`(A..N - 1, *)`}] {
    -      def pure[A](a: A): ${`A0, A(N - 1)&`("A")} = $monadPureMethod
    -    }
    |}
    |private[cats] sealed trait NTupleMonadInstances2 extends NTupleMonadInstances3 {
    -  implicit final def catsStdCommutativeFlatMapForTuple$arity${`[A0, A(N - 1)]`}${`constraints A..(N-1)`(
      "CommutativeSemigroup"
    )}: CommutativeFlatMap[${`(A..N - 1, *)`}] =
    -    new FlatMapTuple$arity${`[A0, A(N - 1)]`}(${`A0, A(N - 1)`}) with CommutativeFlatMap[${`(A..N - 1, *)`}]
    |}
    |private[cats] sealed trait NTupleMonadInstances3 extends NTupleMonadInstances4 {
    -  implicit def catsStdMonadForTuple$arity${`[A0, A(N - 1)]`}${`constraints A..(N-1)`("Monoid")}: Monad[${`(A..N - 1, *)`}] =
    -    new FlatMapTuple$arity${`[A0, A(N - 1)]`}(${`A0, A(N - 1)`}) with Monad[${`(A..N - 1, *)`}] {
    -      def pure[A](a: A): ${`A0, A(N - 1)&`("A")} = $monadPureMethod
    -    }
    |}
    |private[cats] sealed trait NTupleMonadInstances4 {
    -  implicit def catsStdFlatMapForTuple$arity${`[A0, A(N - 1)]`}${`constraints A..(N-1)`("Semigroup")}: FlatMap[${`(A..N - 1, *)`}] =
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
