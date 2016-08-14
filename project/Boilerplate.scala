import sbt._

/**
 * Copied, with some modifications, from https://github.com/milessabin/shapeless/blob/master/project/Boilerplate.scala
 *
 * Generate a range of boilerplate classes, those offering alternatives with 0-22 params
 * and would be tedious to craft by hand
 *
 * @author Miles Sabin
 * @author Kevin Wright
 */


object Boilerplate {
  import scala.StringContext._

  implicit final class BlockHelper(val sc: StringContext) extends AnyVal {
    def block(args: Any*): String = {
      val interpolated = sc.standardInterpolator(treatEscapes, args)
      val rawLines = interpolated split '\n'
      val trimmedLines = rawLines map { _ dropWhile (_.isWhitespace) }
      trimmedLines mkString "\n"
    }
  }


  val templates: Seq[Template] = Seq(
    GenCartesianBuilders,
    GenCartesianArityFunctions,
    GenApplyArityFunctions,
    GenTupleCartesianSyntax
  )

  val header = "// auto-generated boilerplate" // TODO: put something meaningful here?


  /** Returns a seq of the generated files.  As a side-effect, it actually generates them... */
  def gen(dir : File) = for(t <- templates) yield {
    val tgtFile = t.filename(dir)
    IO.write(tgtFile, t.body)
    tgtFile
  }

  val maxArity = 22

  final class TemplateVals(val arity: Int) {
    val synTypes     = (0 until arity) map (n => s"A$n")
    val synVals      = (0 until arity) map (n => s"a$n")
    val synTypedVals = (synVals zip synTypes) map { case (v,t) => v + ":" + t}
    val `A..N`       = synTypes.mkString(", ")
    val `a..n`       = synVals.mkString(", ")
    val `_.._`       = Seq.fill(arity)("_").mkString(", ")
    val `(A..N)`     = if (arity == 1) "Tuple1[A]" else synTypes.mkString("(", ", ", ")")
    val `(_.._)`     = if (arity == 1) "Tuple1[_]" else Seq.fill(arity)("_").mkString("(", ", ", ")")
    val `(a..n)`     = if (arity == 1) "Tuple1(a)" else synVals.mkString("(", ", ", ")")
    val `a:A..n:N`   = synTypedVals mkString ", "
  }

  trait Template {
    def filename(root: File):File
    def content(tv: TemplateVals): String
    def range = 1 to maxArity
    def body: String = {
      def expandInstances(contents: IndexedSeq[Array[String]], acc: Array[String] = Array.empty): Array[String] =
        if (!contents.exists(_ exists(_ startsWith "-")))
          acc map (_.tail)
        else {
          val pre = contents.head takeWhile (_ startsWith "|")
          val instances = contents flatMap {_  dropWhile (_ startsWith "|") takeWhile (_ startsWith "-") }
          val next = contents map {_ dropWhile (_ startsWith "|") dropWhile (_ startsWith "-") }
          expandInstances(next, acc ++ pre ++ instances)
        }

      val rawContents = range map { n => content(new TemplateVals(n)) split '\n' filterNot (_.isEmpty) }
      val headerLines = header split '\n'
      val instances = expandInstances(rawContents)
      val footerLines = rawContents.head.reverse.takeWhile(_ startsWith "|").map(_.tail).reverse
      (headerLines ++ instances ++ footerLines) mkString "\n"
    }
  }


  /*
    Blocks in the templates below use a custom interpolator, combined with post-processing to produce the body

      - The contents of the `header` val is output first

      - Then the first block of lines beginning with '|'

      - Then the block of lines beginning with '-' is replicated once for each arity,
        with the `templateVals` already pre-populated with relevant relevant vals for that arity

      - Then the last block of lines prefixed with '|'

    The block otherwise behaves as a standard interpolated string with regards to variable substitution.
  */

  object GenCartesianBuilders extends Template {
    def filename(root: File) = root /  "cats" / "syntax" / "CartesianBuilder.scala"

    def content(tv: TemplateVals) = {
      import tv._

      val tpes = synTypes map { tpe => s"F[$tpe]" }
      val tpesString = synTypes mkString ", "
      val params = (synVals zip tpes) map { case (v,t) => s"$v:$t"} mkString ", "
      val next = if (arity + 1 <= maxArity) {
        s"def |@|[Z](z: F[Z]) = new CartesianBuilder${arity + 1}(${`a..n`}, z)"
      } else {
        ""
      }

      val n = if (arity == 1) { "" } else { arity.toString }

      val map =
        if (arity == 1) s"def map[Z](f: (${`A..N`}) => Z)(implicit functor: Functor[F]): F[Z] = functor.map(${`a..n`})(f)"
        else s"def map[Z](f: (${`A..N`}) => Z)(implicit functor: Functor[F], cartesian: Cartesian[F]): F[Z] = Cartesian.map$n(${`a..n`})(f)"

      val contramap =
        if (arity == 1) s"def contramap[Z](f: Z => (${`A..N`}))(implicit contravariant: Contravariant[F]): F[Z] = contravariant.contramap(${`a..n`})(f)"
        else s"def contramap[Z](f: Z => (${`A..N`}))(implicit contravariant: Contravariant[F], cartesian: Cartesian[F]): F[Z] = Cartesian.contramap$n(${`a..n`})(f)"

      val imap =
        if (arity == 1) s"def imap[Z](f: (${`A..N`}) => Z)(g: Z => (${`A..N`}))(implicit invariant: Invariant[F]): F[Z] = invariant.imap(${`a..n`})(f)(g)"
        else s"def imap[Z](f: (${`A..N`}) => Z)(g: Z => (${`A..N`}))(implicit invariant: Invariant[F], cartesian: Cartesian[F]): F[Z] = Cartesian.imap$n(${`a..n`})(f)(g)"

      val tupled = if (arity != 1) {
        s"def tupled(implicit invariant: Invariant[F], cartesian: Cartesian[F]): F[(${`A..N`})] = Cartesian.tuple$n(${`a..n`})"
      } else {
        ""
      }

      block"""
        |package cats
        |package syntax
        |
        |import cats.functor.{Contravariant, Invariant}
        |
        |private[syntax] final class CartesianBuilder[F[_]] {
        |  def |@|[A](a: F[A]) = new CartesianBuilder1(a)
        |
        -  private[syntax] final class CartesianBuilder$arity[${`A..N`}]($params) {
        -    $next
        -    def apWith[Z](f: F[(${`A..N`}) => Z])(implicit apply: Apply[F]): F[Z] = apply.ap$n(f)(${`a..n`})
        -    $map
        -    $contramap
        -    $imap
        -    $tupled
        - }
        |}
      """
    }
  }

  object GenApplyArityFunctions extends Template {
    def filename(root: File) = root / "cats" / "ApplyArityFunctions.scala"
    override def range = 3 to maxArity
    def content(tv: TemplateVals) = {
      import tv._

      val tpes = synTypes map { tpe => s"F[$tpe]" }
      val fargs = (0 until arity) map { "f" + _ }
      val fparams = (fargs zip tpes) map { case (v,t) => s"$v:$t"} mkString ", "

      val a = arity / 2
      val b = arity - a

      val fArgsA = (0 until a) map { "f" + _ } mkString ","
      val fArgsB = (a until arity) map { "f" + _ } mkString ","
      val argsA = (0 until a) map { n => "a" + n + ":A" + n } mkString ","
      val argsB = (a until arity) map { n => "a" + n + ":A" + n } mkString ","
      def apN(n: Int) = if (n == 1) { "ap" } else { s"ap$n" }
      def allArgs = (0 until arity) map { "a" + _ } mkString ","

      val apply =
        block"""
          -    ${apN(b)}(${apN(a)}(map(f)(f =>
          -      ($argsA) => ($argsB) => f($allArgs)
          -    ))($fArgsA))($fArgsB)
          """

      block"""
        |package cats
        |trait ApplyArityFunctions[F[_]] { self: Apply[F] =>
        |  def tuple2[A, B](f1: F[A], f2: F[B]): F[(A, B)] = Cartesian.tuple2(f1, f2)(self, self)
        -  def ap$arity[${`A..N`}, Z](f: F[(${`A..N`}) => Z])($fparams):F[Z] = $apply
        -  def map$arity[${`A..N`}, Z]($fparams)(f: (${`A..N`}) => Z): F[Z] = Cartesian.map$arity($fparams)(f)(self, self)
        -  def tuple$arity[${`A..N`}, Z]($fparams): F[(${`A..N`})] = Cartesian.tuple$arity($fparams)(self, self)
        |}
      """
    }
  }

  object GenCartesianArityFunctions extends Template {
    def filename(root: File) = root / "cats" / "CartesianArityFunctions.scala"
    override def range = 2 to maxArity
    def content(tv: TemplateVals) = {
      import tv._

      val tpes = synTypes map { tpe => s"F[$tpe]" }
      val fargs = (0 until arity) map { "f" + _ }
      val fparams = (fargs zip tpes) map { case (v,t) => s"$v:$t"} mkString ", "
      val fargsS = fargs mkString ", "

      val nestedProducts = (0 until (arity - 2)).foldRight(s"cartesian.product(f${arity - 2}, f${arity - 1})")((i, acc) => s"cartesian.product(f$i, $acc)")
      val `nested (a..n)` = (0 until (arity - 2)).foldRight(s"(a${arity - 2}, a${arity - 1})")((i, acc) => s"(a$i, $acc)")

      block"""
         |package cats
         |trait CartesianArityFunctions {
        -  def map$arity[F[_], ${`A..N`}, Z]($fparams)(f: (${`A..N`}) => Z)(implicit cartesian: Cartesian[F], functor: Functor[F]): F[Z] =
        -    functor.map($nestedProducts) { case ${`nested (a..n)`} => f(${`a..n`}) }
        -  def contramap$arity[F[_], ${`A..N`}, Z]($fparams)(f: Z => (${`A..N`}))(implicit cartesian: Cartesian[F], contravariant: functor.Contravariant[F]):F[Z] =
        -    contravariant.contramap($nestedProducts) { z => val ${`(a..n)`} = f(z); ${`nested (a..n)`} }
        -  def imap$arity[F[_], ${`A..N`}, Z]($fparams)(f: (${`A..N`}) => Z)(g: Z => (${`A..N`}))(implicit cartesian: Cartesian[F], invariant: functor.Invariant[F]):F[Z] =
        -    invariant.imap($nestedProducts) { case ${`nested (a..n)`} => f(${`a..n`}) } { z => val ${`(a..n)`} = g(z); ${`nested (a..n)`} }
        -  def tuple$arity[F[_], ${`A..N`}]($fparams)(implicit cartesian: Cartesian[F], invariant: functor.Invariant[F]):F[(${`A..N`})] =
        -    imap$arity($fargsS)((${`_.._`}))(identity)
         |}
      """
    }
  }

  object GenTupleCartesianSyntax extends Template {
    def filename(root: File) = root /  "cats" / "syntax" / "TupleCartesianSyntax.scala"

    def content(tv: TemplateVals) = {
      import tv._

      val tpes = synTypes map { tpe => s"F[$tpe]" }
      val tpesString = tpes mkString ", "

      val tuple = s"Tuple$arity[$tpesString]"
      val tupleTpe = s"t$arity: $tuple"
      val tupleArgs = (1 to arity) map { case n => s"t$arity._$n" } mkString ", "

      val n = if (arity == 1) { "" } else { arity.toString }

      val map =
        if (arity == 1) s"def map[Z](f: (${`A..N`}) => Z)(implicit functor: Functor[F]): F[Z] = functor.map($tupleArgs)(f)"
        else s"def map$arity[Z](f: (${`A..N`}) => Z)(implicit functor: Functor[F], cartesian: Cartesian[F]): F[Z] = Cartesian.map$arity($tupleArgs)(f)"

      val contramap =
        if (arity == 1) s"def contramap[Z](f: Z => (${`A..N`}))(implicit contravariant: Contravariant[F]): F[Z] = contravariant.contramap($tupleArgs)(f)"
        else s"def contramap$arity[Z](f: Z => (${`A..N`}))(implicit contravariant: Contravariant[F], cartesian: Cartesian[F]): F[Z] = Cartesian.contramap$arity($tupleArgs)(f)"

      val imap =
        if (arity == 1) s"def imap[Z](f: (${`A..N`}) => Z)(g: Z => (${`A..N`}))(implicit invariant: Invariant[F]): F[Z] = invariant.imap($tupleArgs)(f)(g)"
        else s"def imap$arity[Z](f: (${`A..N`}) => Z)(g: Z => (${`A..N`}))(implicit invariant: Invariant[F], cartesian: Cartesian[F]): F[Z] = Cartesian.imap$arity($tupleArgs)(f)(g)"

      block"""
        |package cats
        |package syntax
        |
        |import cats.functor.{Contravariant, Invariant}
        |
        |trait TupleCartesianSyntax {
        -  implicit def catsSyntaxTuple${arity}Cartesian[F[_], ${`A..N`}]($tupleTpe): Tuple${arity}CartesianOps[F, ${`A..N`}] = new Tuple${arity}CartesianOps(t$arity)
        |}
        |
        -private[syntax] final class Tuple${arity}CartesianOps[F[_], ${`A..N`}]($tupleTpe) {
        -  $map
        -  $contramap
        -  $imap
        -  def apWith[Z](f: F[(${`A..N`}) => Z])(implicit apply: Apply[F]): F[Z] = apply.ap$n(f)($tupleArgs)
        -}
        |
      """
    }
  }

}
