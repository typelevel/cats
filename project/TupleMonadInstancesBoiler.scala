import sbt.*

import Boilerplate.*
import Boilerplate.{Template, TemplateVals}
import sbt.File

object GenTupleMonadInstances extends Template {
  override def range = 1 to 11
  override def filename(root: sbt.File): File =
    root / "cats" / "instances" / "NTupleMonadInstances.scala"

  override def content(tv: TemplateVals): String = {
    import tv.*

    val `b..(n - 1)` = for (n <- 0 until arity - 1) yield s"b$n"
    val `b:A..(n - 1):(N - 1)` = (for ((v, t) <- `b..(n - 1)`.zip(`A..(N - 1)`)) yield s"$v: $t").mkString(", ")

    /**
     * This special case for N = 2 is needed because
     * of the deprecated versions in TupleInstances.
     * It will be removed once the deprecated ones are
     * deleted.
     */
    val flatMapTupleClass = if (arity == 2) "FlatMapNTuple2" else s"FlatMapTuple$arity"

    def `combine A..(N - 1)`(a: String, b: String, last: String): String =
      if (arity <= 1)
        s"Tuple1($last)"
      else
        `A..(N - 1)`.iterator.zipWithIndex
          .map { case (an, i) => s"$an.combine($a._${i + 1}, $b._${i + 1})" }
          .mkString("(", ", ", s", $last)")

    val monadPureMethod: String =
      if (arity <= 1) "Tuple1(a)"
      else `A..(N - 1)`.map(n => s"$n.empty").mkString("(", ", ", ", a)")

    val tailRecMCombine =
      `A..(N - 1)`.iterator.zipWithIndex
        .map { case (an, i) => s"$an.combine(b$i, a$i)" }
        .mkString(", ")

    val tailRecMMethod =
      if (arity == 1)
        block"""
        -    @tailrec
        -    def loop(a: A): Tuple1[B] =
        -      f(a) match {
        -        case Tuple1(Right(b))    => Tuple1(b)
        -        case Tuple1(Left(nextA)) => loop(nextA)
        -      }
        -    loop(a)
        """
      else
        block"""
        -    @tailrec
        -    def loop(${`b:A..(n - 1):(N - 1)`}, a: A): ${`A0, A(N - 1)&`("B")} =
        -      f(a) match {
        -        case (${`a0, a(n - 1)`}, Right(b))    => ($tailRecMCombine, b)
        -        case (${`a0, a(n - 1)`}, Left(nextA)) => loop($tailRecMCombine, nextA)
        -      }
        -    f(a) match {
        -      case (${`a0, a(n - 1)`}, Right(b))    => (${`a0, a(n - 1)`}, b)
        -      case (${`a0, a(n - 1)`}, Left(nextA)) => loop(${`a0, a(n - 1)`}, nextA)
        -    }
        """

    val `:CommutativeMonoid` = `constraints A..(N-1)`("CommutativeMonoid")
    val `:CommutativeSemigroup` = `constraints A..(N-1)`("CommutativeSemigroup")
    val `:Monoid` = `constraints A..(N-1)`("Monoid")
    val `:Semigroup` = `constraints A..(N-1)`("Semigroup")

    block"""
    |
    |package cats
    |package instances
    |
    |import cats.kernel.{CommutativeMonoid, CommutativeSemigroup}
    |import scala.annotation.tailrec
    |
    |private[cats] trait NTupleMonadInstances extends NTupleMonadInstances1 {
    |
    |  private def instance[F[_] <: Product](cofMap: (F[Any], F[Any] => Any) => F[Any]): Comonad[F] =
    |    new Comonad[F] {
    |      def coflatMap[A, B](fa: F[A])(f: F[A] => B) =
    |        cofMap(fa.asInstanceOf[F[Any]], f.asInstanceOf[F[Any] => Any]).asInstanceOf[F[B]]
    |      def extract[A](fa: F[A]) =
    |        fa.productElement(fa.productArity - 1).asInstanceOf[A]
    |      def map[A, B](fa: F[A])(f: A => B) =
    |        coflatMap(fa)(fa => f(extract(fa)))
    |    }
    -
    -  implicit final def catsStdInstancesForTuple$arity${`[A0, A(N - 1)]`}: Comonad[${`(A..N - 1, *)`}] =
    -    instance((fa, f) => fa.copy(_$arity = f(fa)))
    |}
    |private[cats] sealed trait NTupleMonadInstances1 extends NTupleMonadInstances2 { this: NTupleMonadInstances =>
    -  implicit final def catsStdCommutativeMonadForTuple$arity${`[A0, A(N - 1)]`}${`:CommutativeMonoid`}: CommutativeMonad[${`(A..N - 1, *)`}] =
    -    new FlatMapTuple$arity${`[A0, A(N - 1)]`}(${`A0, A(N - 1)`}) with CommutativeMonad[${`(A..N - 1, *)`}] {
    -      def pure[A](a: A) = $monadPureMethod
    -    }
    |}
    |private[cats] sealed trait NTupleMonadInstances2 extends NTupleMonadInstances3 { this: NTupleMonadInstances =>
    -  implicit final def catsStdCommutativeFlatMapForTuple$arity${`[A0, A(N - 1)]`}${`:CommutativeSemigroup`}: CommutativeFlatMap[${`(A..N - 1, *)`}] =
    -    new FlatMapTuple$arity${`[A0, A(N - 1)]`}(${`A0, A(N - 1)`}) with CommutativeFlatMap[${`(A..N - 1, *)`}]
    |}
    |private[cats] sealed trait NTupleMonadInstances3 extends NTupleMonadInstances4 { this: NTupleMonadInstances =>
    -  implicit def catsStdMonadForTuple$arity${`[A0, A(N - 1)]`}${`:Monoid`}: Monad[${`(A..N - 1, *)`}] =
    -    new FlatMapTuple$arity${`[A0, A(N - 1)]`}(${`A0, A(N - 1)`}) with Monad[${`(A..N - 1, *)`}] {
    -      def pure[A](a: A) = $monadPureMethod
    -    }
    |}
    |private[cats] sealed trait NTupleMonadInstances4 extends NTupleMonadInstances5 { this: NTupleMonadInstances =>
    -  implicit def catsStdFlatMapForTuple$arity${`[A0, A(N - 1)]`}${`:Semigroup`}: FlatMap[${`(A..N - 1, *)`}] =
    -    new FlatMapTuple$arity${`[A0, A(N - 1)]`}(${`A0, A(N - 1)`})
    |}
    |private[cats] sealed trait NTupleMonadInstances5 { this: NTupleMonadInstances =>
    -  implicit def catsStdInvariantForTuple$arity${`[A0, A(N - 1)]`}: Invariant[${`(A..N - 1, *)`}] =
    -    catsStdInstancesForTuple$arity
    |}
    -
    -private[instances] class $flatMapTupleClass${`[A0, A(N - 1)]`}(${`parameters A..(N-1)`("Semigroup")})
    -    extends FlatMap[${`(A..N - 1, *)`}] {
    -  override def ap[A, B](ff: ${`A0, A(N - 1)&`("A => B")})(fa: ${`A0, A(N - 1)&`("A")}) =
    -    ${`combine A..(N - 1)`("ff", "fa", s"ff._$arity(fa._$arity)")}
    -  override def product[A, B](fa: ${`A0, A(N - 1)&`("A")}, fb: ${`A0, A(N - 1)&`("B")}) =
    -    ${`combine A..(N - 1)`("fa", "fb", s"(fa._$arity, fb._$arity)")}
    -  override def map[A, B](fa: ${`A0, A(N - 1)&`("A")})(f: A => B) =
    -    fa.copy(_$arity = f(fa._$arity))
    -  def flatMap[A, B](fa: ${`A0, A(N - 1)&`("A")})(f: A => ${`A0, A(N - 1)&`("B")}) = {
    -    val xb = f(fa._$arity)
    -    ${`combine A..(N - 1)`("fa", "xb", s"xb._$arity")}
    -  }
    -  override def productR[A, B](a: ${`A0, A(N - 1)&`("A")})(b: ${`A0, A(N - 1)&`("B")}) =
    -    ${`combine A..(N - 1)`("a", "b", s"b._$arity")}
    -  override def productL[A, B](a: ${`A0, A(N - 1)&`("A")})(b: ${`A0, A(N - 1)&`("B")}) =
    -    ${`combine A..(N - 1)`("a", "b", s"a._$arity")}
    -  override def mproduct[A, B](fa: ${`A0, A(N - 1)&`("A")})(f: A => ${`A0, A(N - 1)&`("B")}) = {
    -    val xb = f(fa._$arity)
    -    ${`combine A..(N - 1)`("fa", "xb", s"(fa._$arity, xb._$arity)")}
    -  }
    -  def tailRecM[A, B](a: A)(f: A => ${`A0, A(N - 1)&`("Either[A, B]")}) = {
    -    $tailRecMMethod
    -  }
    -}"""
  }
}
