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

  implicit final class BlockHelper(private val sc: StringContext) extends AnyVal {
    def block(args: Any*): String = {
      val interpolated = sc.standardInterpolator(treatEscapes, args)
      val rawLines = interpolated.split('\n')
      val trimmedLines = rawLines.map(_.dropWhile(_.isWhitespace))
      trimmedLines.mkString("\n")
    }
  }

  val templates: Seq[Template] = Seq(
    GenSemigroupalBuilders,
    GenSemigroupalArityFunctions,
    GenApplyArityFunctions,
    GenTupleSemigroupalSyntax,
    GenParallelArityFunctions,
    GenParallelArityFunctions2,
    GenTupleParallelSyntax,
    GenFoldableArityFunctions
  )

  val header = "// auto-generated boilerplate by /project/Boilerplate.scala" // TODO: put something meaningful here?

  /**
   * Returns a seq of the generated files.  As a side-effect, it actually generates them...
   */
  def gen(dir: File) =
    for (t <- templates) yield {
      val tgtFile = t.filename(dir)
      IO.write(tgtFile, t.body)
      tgtFile
    }

  val maxArity = 22

  final class TemplateVals(val arity: Int) {
    val synTypes = (0 until arity).map(n => s"A$n")
    val synVals = (0 until arity).map(n => s"a$n")
    val synTypedVals = (synVals.zip(synTypes)).map { case (v, t) => v + ":" + t }
    val `A..N` = synTypes.mkString(", ")
    val `a..n` = synVals.mkString(", ")
    val `_.._` = Seq.fill(arity)("_").mkString(", ")
    val `(A..N)` = if (arity == 1) "Tuple1[A]" else synTypes.mkString("(", ", ", ")")
    val `(_.._)` = if (arity == 1) "Tuple1[_]" else Seq.fill(arity)("_").mkString("(", ", ", ")")
    val `(a..n)` = if (arity == 1) "Tuple1(a)" else synVals.mkString("(", ", ", ")")
    val `a:A..n:N` = synTypedVals.mkString(", ")
  }

  trait Template {
    def filename(root: File): File
    def content(tv: TemplateVals): String
    def range = 1 to maxArity
    def body: String = {
      def expandInstances(contents: IndexedSeq[Array[String]], acc: Array[String] = Array.empty): Array[String] =
        if (!contents.exists(_.exists(_.startsWith("-"))))
          acc.map(_.tail)
        else {
          val pre = contents.head.takeWhile(_.startsWith("|"))
          val instances = contents.flatMap(_.dropWhile(_.startsWith("|")).takeWhile(_.startsWith("-")))
          val next = contents.map(_.dropWhile(_.startsWith("|")).dropWhile(_.startsWith("-")))
          expandInstances(next, acc ++ pre ++ instances)
        }

      val rawContents = range.map { n =>
        content(new TemplateVals(n)).split('\n').filterNot(_.isEmpty)
      }
      val headerLines = header.split('\n')
      val instances = expandInstances(rawContents)
      val footerLines = rawContents.head.reverse.takeWhile(_.startsWith("|")).map(_.tail).reverse
      (headerLines ++ instances ++ footerLines).mkString("\n")
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

  object GenSemigroupalBuilders extends Template {
    def filename(root: File) = root / "cats" / "syntax" / "SemigroupalBuilder.scala"

    def content(tv: TemplateVals) = {
      import tv._

      val tpes = synTypes.map { tpe =>
        s"F[$tpe]"
      }
      val tpesString = synTypes.mkString(", ")
      val params = (synVals.zip(tpes)).map { case (v, t) => s"$v:$t" }.mkString(", ")
      val next = if (arity + 1 <= maxArity) {
        s"def |@|[Z](z: F[Z]) = new SemigroupalBuilder${arity + 1}(${`a..n`}, z)"
      } else {
        ""
      }

      val n = if (arity == 1) {
        ""
      } else {
        arity.toString
      }

      val map =
        if (arity == 1)
          s"def map[Z](f: (${`A..N`}) => Z)(implicit functor: Functor[F]): F[Z] = functor.map(${`a..n`})(f)"
        else
          s"def map[Z](f: (${`A..N`}) => Z)(implicit functor: Functor[F], semigroupal: Semigroupal[F]): F[Z] = Semigroupal.map$n(${`a..n`})(f)"

      val contramap =
        if (arity == 1)
          s"def contramap[Z](f: Z => (${`A..N`}))(implicit contravariant: Contravariant[F]): F[Z] = contravariant.contramap(${`a..n`})(f)"
        else
          s"def contramap[Z](f: Z => (${`A..N`}))(implicit contravariant: Contravariant[F], semigroupal: Semigroupal[F]): F[Z] = Semigroupal.contramap$n(${`a..n`})(f)"

      val imap =
        if (arity == 1)
          s"def imap[Z](f: (${`A..N`}) => Z)(g: Z => (${`A..N`}))(implicit invariant: Invariant[F]): F[Z] = invariant.imap(${`a..n`})(f)(g)"
        else
          s"def imap[Z](f: (${`A..N`}) => Z)(g: Z => (${`A..N`}))(implicit invariant: Invariant[F], semigroupal: Semigroupal[F]): F[Z] = Semigroupal.imap$n(${`a..n`})(f)(g)"

      val tupled = if (arity != 1) {
        s"def tupled(implicit invariant: Invariant[F], semigroupal: Semigroupal[F]): F[(${`A..N`})] = Semigroupal.tuple$n(${`a..n`})"
      } else {
        ""
      }

      block"""
      |package cats
      |package syntax
      |
      |
      |
      |@deprecated("replaced by apply syntax", "1.0.0-MF")
      |private[syntax] final class SemigroupalBuilder[F[_]] extends Serializable {
      |  def |@|[A](a: F[A]) = new SemigroupalBuilder1(a)
      |
        -  private[syntax] final class SemigroupalBuilder$arity[${`A..N`}]($params) extends Serializable {
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

      val tpes = synTypes.map { tpe =>
        s"F[$tpe]"
      }
      val fargs = (0 until arity).map("f" + _)
      val fparams = (fargs.zip(tpes)).map { case (v, t) => s"$v:$t" }.mkString(", ")

      val a = arity / 2
      val b = arity - a

      val fArgsA = (0 until a).map("f" + _).mkString(",")
      val fArgsB = (a until arity).map("f" + _).mkString(",")
      val argsA = (0 until a)
        .map { n =>
          "a" + n + ":A" + n
        }
        .mkString(",")
      val argsB = (a until arity)
        .map { n =>
          "a" + n + ":A" + n
        }
        .mkString(",")
      def apN(n: Int) =
        if (n == 1) {
          "ap"
        } else {
          s"ap$n"
        }
      def allArgs = (0 until arity).map("a" + _).mkString(",")

      val apply =
        block"""
          -    ${apN(b)}(${apN(a)}(map(f)(f =>
          -      ($argsA) => ($argsB) => f($allArgs)
          -    ))($fArgsA))($fArgsB)
          """

      block"""
      |package cats
      |
      |/**
      | * @groupprio Ungrouped 0
      | *
      | * @groupname ApArity ap arity
      | * @groupdesc ApArity Higher-arity ap methods
      | * @groupprio ApArity 999
      | *
      | * @groupname MapArity map arity
      | * @groupdesc MapArity Higher-arity map methods
      | * @groupprio MapArity 999
      | *
      | * @groupname TupleArity tuple arity
      | * @groupdesc TupleArity Higher-arity tuple methods
      | * @groupprio TupleArity 999
      | */
      |trait ApplyArityFunctions[F[_]] { self: Apply[F] =>
      |  def tuple2[A, B](f1: F[A], f2: F[B]): F[(A, B)] = Semigroupal.tuple2(f1, f2)(self, self)
        -  /** @group ApArity */
        -  def ap$arity[${`A..N`}, Z](f: F[(${`A..N`}) => Z])($fparams):F[Z] = $apply
        -  /** @group MapArity */
        -  def map$arity[${`A..N`}, Z]($fparams)(f: (${`A..N`}) => Z): F[Z] = Semigroupal.map$arity($fparams)(f)(self, self)
        -  /** @group TupleArity */
        -  def tuple$arity[${`A..N`}, Z]($fparams): F[(${`A..N`})] = Semigroupal.tuple$arity($fparams)(self, self)
      |}
      """
    }
  }

  final case class ParallelNestedExpansions(arity: Int) {
    val products = (0 until (arity - 2))
      .foldRight(s"Parallel.parProduct(m${arity - 2}, m${arity - 1})")((i, acc) => s"Parallel.parProduct(m$i, $acc)")
    val `(a..n)` = (0 until (arity - 2)).foldRight(s"(a${arity - 2}, a${arity - 1})")((i, acc) => s"(a$i, $acc)")
  }

  object GenParallelArityFunctions extends Template {
    def filename(root: File) = root / "cats" / "ParallelArityFunctions.scala"
    override def range = 2 to maxArity
    def content(tv: TemplateVals) = {
      import tv._

      val tpes = synTypes.map { tpe =>
        s"M[$tpe]"
      }
      val fargs = (0 until arity).map("m" + _)
      val fparams = (fargs.zip(tpes)).map { case (v, t) => s"$v:$t" }.mkString(", ")
      val fargsS = fargs.mkString(", ")
      val nestedExpansion = ParallelNestedExpansions(arity)

      block"""
      |package cats
      |
      |/**
      | * @groupprio Ungrouped 0
      | *
      | * @groupname ParMapArity parMap arity
      | * @groupdesc ParMapArity Higher-arity parMap methods
      | * @groupprio ParMapArity 999
      | */
      |trait ParallelArityFunctions {
        -  /** @group ParMapArity */
        -  def parMap$arity[M[_], ${`A..N`}, Z]($fparams)(f: (${`A..N`}) => Z)(implicit p: NonEmptyParallel[M]): M[Z] =
        -    p.flatMap.map(${nestedExpansion.products}) { case ${nestedExpansion.`(a..n)`} => f(${`a..n`}) }
      |}
      """
    }
  }

  object GenParallelArityFunctions2 extends Template {
    def filename(root: File) = root / "cats" / "ParallelArityFunctions2.scala"
    override def range = 2 to maxArity
    def content(tv: TemplateVals) = {
      import tv._

      val tpes = synTypes.map { tpe =>
        s"M[$tpe]"
      }
      val fargs = (0 until arity).map("m" + _)
      val fparams = (fargs.zip(tpes)).map { case (v, t) => s"$v:$t" }.mkString(", ")
      val fargsS = fargs.mkString(", ")
      val nestedExpansion = ParallelNestedExpansions(arity)

      block"""
      |package cats
      |
      |/**
      | * @groupprio Ungrouped 0
      | *
      | * @groupname ParTupleArity parTuple arity
      | * @groupdesc ParTupleArity Higher-arity parTuple methods
      | * @groupprio ParTupleArity 999
      | */
      |abstract class ParallelArityFunctions2 extends ParallelArityFunctions {
        -  /** @group ParTupleArity */
        -  def parTuple$arity[M[_], ${`A..N`}]($fparams)(implicit p: NonEmptyParallel[M]): M[(${`A..N`})] =
        -    p.flatMap.map(${nestedExpansion.products}) { case ${nestedExpansion.`(a..n)`} => (${`a..n`}) }
      |}
      """
    }
  }

  object GenSemigroupalArityFunctions extends Template {
    def filename(root: File) = root / "cats" / "SemigroupalArityFunctions.scala"
    override def range = 2 to maxArity
    def content(tv: TemplateVals) = {
      import tv._

      val tpes = synTypes.map { tpe =>
        s"F[$tpe]"
      }
      val fargs = (0 until arity).map("f" + _)
      val fparams = (fargs.zip(tpes)).map { case (v, t) => s"$v:$t" }.mkString(", ")
      val fargsS = fargs.mkString(", ")

      val nestedProducts = (0 until (arity - 2))
        .foldRight(s"semigroupal.product(f${arity - 2}, f${arity - 1})")((i, acc) => s"semigroupal.product(f$i, $acc)")
      val `nested (a..n)` =
        (0 until (arity - 2)).foldRight(s"(a${arity - 2}, a${arity - 1})")((i, acc) => s"(a$i, $acc)")

      block"""
      |package cats
      |
      |/**
      | * @groupprio Ungrouped 0
      | *
      | * @groupname MapArity map arity
      | * @groupdesc MapArity Higher-arity map methods
      | * @groupprio MapArity 999
      | *
      | * @groupname ContramapArity contramap arity
      | * @groupdesc ContramapArity Higher-arity contramap methods
      | * @groupprio ContramapArity 999
      | *
      | * @groupname ImapArity imap arity
      | * @groupdesc ImapArity Higher-arity imap methods
      | * @groupprio ImapArity 999
      | *
      | * @groupname TupleArity tuple arity
      | * @groupdesc TupleArity Higher-arity tuple methods
      | * @groupprio TupleArity 999
      | *
      | * @groupname TraverseArity traverse arity
      | * @groupdesc TraverseArity Higher-arity traverse methods
      | * @groupprio TraverseArity 999
      | */
      |trait SemigroupalArityFunctions {
        -  /** @group MapArity */
        -  def map$arity[F[_], ${`A..N`}, Z]($fparams)(f: (${`A..N`}) => Z)(implicit semigroupal: Semigroupal[F], functor: Functor[F]): F[Z] =
        -    functor.map($nestedProducts) { case ${`nested (a..n)`} => f(${`a..n`}) }
        -  /** @group ContramapArity */
        -  def contramap$arity[F[_], ${`A..N`}, Z]($fparams)(f: Z => (${`A..N`}))(implicit semigroupal: Semigroupal[F], contravariant: Contravariant[F]):F[Z] =
        -    contravariant.contramap($nestedProducts) { z => val ${`(a..n)`} = f(z); ${`nested (a..n)`} }
        -  /** @group ImapArity */
        -  def imap$arity[F[_], ${`A..N`}, Z]($fparams)(f: (${`A..N`}) => Z)(g: Z => (${`A..N`}))(implicit semigroupal: Semigroupal[F], invariant: Invariant[F]):F[Z] =
        -    invariant.imap($nestedProducts) { case ${`nested (a..n)`} => f(${`a..n`}) } { z => val ${`(a..n)`} = g(z); ${`nested (a..n)`} }
        -  /** @group TupleArity */
        -  def tuple$arity[F[_], ${`A..N`}]($fparams)(implicit semigroupal: Semigroupal[F], invariant: Invariant[F]):F[(${`A..N`})] =
        -    imap$arity($fargsS)((${`_.._`}))(identity)
        -  /** @group TraverseArity */
        -  def traverse$arity[F[_], G[_], ${`A..N`}, Z]($fparams)(f: (${`A..N`}) => G[Z])(implicit semigroupal: Semigroupal[F], traverse: Traverse[F], applicative: Applicative[G]): G[F[Z]] =
        -    traverse.traverse($nestedProducts) { case ${`nested (a..n)`} => f(${`a..n`}) }
      |}
      """
    }
  }

  object GenTupleParallelSyntax extends Template {
    def filename(root: File) = root / "cats" / "syntax" / "TupleParallelSyntax.scala"

    def content(tv: TemplateVals) = {
      import tv._

      val tpes = synTypes.map { tpe =>
        s"M[$tpe]"
      }
      val tpesString = tpes.mkString(", ")

      val tuple = s"Tuple$arity[$tpesString]"
      val tupleTpe = s"t$arity: $tuple"
      val tupleArgs = (1 to arity).map { case n => s"t$arity._$n" }.mkString(", ")

      val n = if (arity == 1) {
        ""
      } else {
        arity.toString
      }

      val parMap =
        if (arity == 1)
          s"def parMap[Z](f: (${`A..N`}) => Z)(implicit p: NonEmptyParallel[M]): M[Z] = p.flatMap.map($tupleArgs)(f)"
        else
          s"def parMapN[Z](f: (${`A..N`}) => Z)(implicit p: NonEmptyParallel[M]): M[Z] = Parallel.parMap$arity($tupleArgs)(f)"

      val parTupled =
        if (arity == 1) ""
        else
          s"def parTupled(implicit p: NonEmptyParallel[M]): M[(${`A..N`})] = Parallel.parTuple$arity($tupleArgs)"

      block"""
      |package cats
      |package syntax
      |
      |import cats.Parallel
      |
      |trait TupleParallelSyntax {
         -  implicit def catsSyntaxTuple${arity}Parallel[M[_], ${`A..N`}]($tupleTpe): Tuple${arity}ParallelOps[M, ${`A..N`}] = new Tuple${arity}ParallelOps(t$arity)
      |}
      |
         -private[syntax] final class Tuple${arity}ParallelOps[M[_], ${`A..N`}](private val $tupleTpe) extends Serializable {
         -  $parMap
         -  $parTupled
         -}
      |
      """
    }
  }

  object GenTupleSemigroupalSyntax extends Template {
    def filename(root: File) = root / "cats" / "syntax" / "TupleSemigroupalSyntax.scala"

    def content(tv: TemplateVals) = {
      import tv._

      val tpes = synTypes.map { tpe =>
        s"F[$tpe]"
      }
      val tpesString = tpes.mkString(", ")

      val tuple = s"Tuple$arity[$tpesString]"
      val tupleTpe = s"t$arity: $tuple"
      val tupleArgs = (1 to arity).map { case n => s"t$arity._$n" }.mkString(", ")

      val n = if (arity == 1) {
        ""
      } else {
        arity.toString
      }

      val map =
        if (arity == 1)
          s"def map[Z](f: (${`A..N`}) => Z)(implicit functor: Functor[F]): F[Z] = functor.map($tupleArgs)(f)"
        else
          s"def mapN[Z](f: (${`A..N`}) => Z)(implicit functor: Functor[F], semigroupal: Semigroupal[F]): F[Z] = Semigroupal.map$arity($tupleArgs)(f)"

      val contramap =
        if (arity == 1)
          s"def contramap[Z](f: Z => (${`A..N`}))(implicit contravariant: Contravariant[F]): F[Z] = contravariant.contramap($tupleArgs)(f)"
        else
          s"def contramapN[Z](f: Z => (${`A..N`}))(implicit contravariant: Contravariant[F], semigroupal: Semigroupal[F]): F[Z] = Semigroupal.contramap$arity($tupleArgs)(f)"

      val imap =
        if (arity == 1)
          s"def imap[Z](f: (${`A..N`}) => Z)(g: Z => (${`A..N`}))(implicit invariant: Invariant[F]): F[Z] = invariant.imap($tupleArgs)(f)(g)"
        else
          s"def imapN[Z](f: (${`A..N`}) => Z)(g: Z => (${`A..N`}))(implicit invariant: Invariant[F], semigroupal: Semigroupal[F]): F[Z] = Semigroupal.imap$arity($tupleArgs)(f)(g)"

      val tupled = if (arity != 1) {
        s"def tupled(implicit invariant: Invariant[F], semigroupal: Semigroupal[F]): F[(${`A..N`})] = Semigroupal.tuple$n($tupleArgs)"
      } else {
        ""
      }

      val traverse =
        if (arity == 1)
          s"def traverse[G[_]: Applicative, Z](f: (${`A..N`}) => G[Z])(implicit traverse: Traverse[F]): G[F[Z]] = traverse.traverse($tupleArgs)(f)"
        else
          s"def traverseN[G[_]: Applicative, Z](f: (${`A..N`}) => G[Z])(implicit traverse: Traverse[F], semigroupal: Semigroupal[F]): G[F[Z]] = Semigroupal.traverse$arity($tupleArgs)(f)"

      block"""
      |package cats
      |package syntax
      |
      |
      |
      |trait TupleSemigroupalSyntax {
        -  implicit def catsSyntaxTuple${arity}Semigroupal[F[_], ${`A..N`}]($tupleTpe): Tuple${arity}SemigroupalOps[F, ${`A..N`}] = new Tuple${arity}SemigroupalOps(t$arity)
      |}
      |
        -private[syntax] final class Tuple${arity}SemigroupalOps[F[_], ${`A..N`}](private val $tupleTpe) extends Serializable {
        -  $map
        -  $contramap
        -  $imap
        -  $tupled
        -  $traverse
        -  def apWith[Z](f: F[(${`A..N`}) => Z])(implicit apply: Apply[F]): F[Z] = apply.ap$n(f)($tupleArgs)
        -}
      |
      """
    }
  }

  object GenFoldableArityFunctions extends Template {
    def filename(root: File) = root / "cats" / "FoldableArityFunctions.scala"
    override def range = 3 to maxArity
    def content(tv: TemplateVals) = {
      import tv._

      val tupleTpe = (1 to arity).map(_ => "A").mkString("(", ", ", ")")
      def listXN(range: Range) = range.map("x" + _).mkString(" :: ")
      val reverseXN = listXN(1 to arity - 1)
      val tupleXN = (1 to arity).map("x" + _).mkString("(", ", ", ")")

      block"""
      |package cats
      |
      |/**
      | * @groupprio Ungrouped 0
      | *
      | * @groupname FoldableSlidingN foldable arity
      | * @groupdesc FoldableSlidingN Sliding windows of size N
      | * @groupprio FoldableSlidingN 999
      | *
      | */
      |trait FoldableArityFunctions[F[_]] { self: Foldable[F] =>
        -  /** @group FoldableArity */
        -  def sliding$arity[A](fa: F[A]): List[$tupleTpe] =
        -    foldRight(fa, Now((List.empty[$tupleTpe], List.empty[A]))) { (x1, eval) =>
        -      eval.value match {
        -        case (acc, ${listXN(2 to arity)} :: Nil) =>
        -          Now(($tupleXN :: acc, ${listXN(1 to arity - 1)} :: Nil))
        -        case (acc, l) =>
        -          Now((acc, x1 :: l))
        -      }
        -    }.value._1
      |}
      """
    }
  }
}
