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

  implicit class BlockHelper(val sc: StringContext) extends AnyVal {
    def block(args: Any*): String = {
      val interpolated = sc.standardInterpolator(treatEscapes, args)
      val rawLines = interpolated split '\n'
      val trimmedLines = rawLines map { _ dropWhile (_.isWhitespace) }
      trimmedLines mkString "\n"
    }
  }


  val templates: Seq[Template] = Seq(
    GenApplyBuilders,
    GenApplyArityFunctions
  )

  val header = "// auto-generated boilerplate" // TODO: put something meaningful here?


  /** Returns a seq of the generated files.  As a side-effect, it actually generates them... */
  def gen(dir : File) = for(t <- templates) yield {
    val tgtFile = t.filename(dir)
    IO.write(tgtFile, t.body)
    tgtFile
  }

  val maxArity = 22

  class TemplateVals(val arity: Int) {
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
      val headerLines = header split '\n'
      val rawContents = range map { n => content(new TemplateVals(n)) split '\n' filterNot (_.isEmpty) }
      val preBody = rawContents.head takeWhile (_ startsWith "|") map (_.tail)
      val instances = rawContents flatMap {_ filter (_ startsWith "-") map (_.tail) }
      val postBody = rawContents.head dropWhile (_ startsWith "|") dropWhile (_ startsWith "-") map (_.tail)
      (headerLines ++ preBody ++ instances ++ postBody) mkString "\n"
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

  object GenApplyBuilders extends Template {
    def filename(root: File) = root /  "cats" / "syntax" / "ApplyBuilder.scala"

    def content(tv: TemplateVals) = {
      import tv._

      val tpes = synTypes map { tpe => s"F[$tpe]" }
      val tpesString = synTypes mkString ", "
      val params = (synVals zip tpes) map { case (v,t) => s"$v:$t"} mkString ", "
      val next = if (arity + 1 <= maxArity) {
        s"def |@|[Z](z: F[Z]) = new ApplyBuilder${arity + 1}(${`a..n`}, z)"
      } else {
        ""
      }

      val n = if (arity == 1) { "" } else { arity.toString }

      val tupled = if (arity != 1) {
        s"def tupled(implicit F: Apply[F]): F[(${`A..N`})] = F.tuple$n(${`a..n`})"
      } else {
        ""
      }

      block"""
        |package cats
        |package syntax
        |
        |private[syntax] class ApplyBuilder[F[_]] {
        |  def |@|[A](a: F[A]) = new ApplyBuilder1(a)
        |
        -  private[syntax] class ApplyBuilder$arity[${`A..N`}](${params}) {
        -    $next
        -    def ap[Z](f: F[(${`A..N`}) => Z])(implicit F: Apply[F]): F[Z] = F.ap$n(${`a..n`})(f)
        -    def map[Z](f: (${`A..N`}) => Z)(implicit F: Apply[F]): F[Z] = F.map$n(${`a..n`})(f)
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
      val tpesString = synTypes mkString ", "
      val fargs = (0 until arity) map { "f" + _ }
      val fargsS = fargs mkString ", "
      val fparams = (fargs zip tpes) map { case (v,t) => s"$v:$t"} mkString ", "

      val a = arity / 2
      val b = arity - a

      val fArgsA = (0 until a) map { "f" + _ } mkString ","
      val fArgsB = (a until arity) map { "f" + _ } mkString ","
      val argsA = (0 until a) map { "a" + _ } mkString ","
      val argsB = (a until arity) map { "a" + _ } mkString ","
      def apN(n: Int) = if (n == 1) { "ap" } else { s"ap$n" }
      def allArgs = (0 until arity) map { "a" + _ } mkString ","

      val map = if (arity == 3) {
        " ap(f2)(map2(f0, f1)((a, b) => c => f(a, b, c)))"
      }  else {
        block"""
          -    map2(tuple$a($fArgsA), tuple$b($fArgsB)) {
          -      case (($argsA), ($argsB)) => f($allArgs)
          -    }
        """
      }
      val apply =
        block"""
          -    ${apN(b)}($fArgsB)(${apN(a)}($fArgsA)(map(f)(f =>
          -      ($argsA) => ($argsB) => f($allArgs)
          -    )))
          """

      block"""
        |package cats
        |trait ApplyArityFunctions[F[_]] { self: Apply[F] =>
        |  def tuple2[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((_, _))
        |
        -  def ap$arity[${`A..N`}, Z]($fparams)(f: F[(${`A..N`}) => Z]):F[Z] = $apply
        -  def map$arity[${`A..N`}, Z]($fparams)(f: (${`A..N`}) => Z):F[Z] = $map
        -  def tuple$arity[${`A..N`}]($fparams):F[(${`A..N`})] = 
        -    map$arity($fargsS)((${`_.._`}))
        |}
      """
    }
  }

}
