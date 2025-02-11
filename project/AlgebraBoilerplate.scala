import sbt.*

/**
 * Generate a range of boilerplate classes that would be tedious to write and maintain by hand.
 *
 * Copied, with some modifications, from
 * [[https://github.com/milessabin/shapeless/blob/master/project/Boilerplate.scala Shapeless]].
 *
 * @author Miles Sabin
 * @author Kevin Wright
 */
object AlgebraBoilerplate {
  import scala.StringContext.*

  implicit class BlockHelper(val sc: StringContext) extends AnyVal {
    def block(args: Any*): String = {
      val interpolated = sc.standardInterpolator(treatEscapes, args)
      val rawLines = interpolated.split('\n')
      val trimmedLines = rawLines.map(_.dropWhile(_.isWhitespace))
      trimmedLines.mkString("\n")
    }
  }

  val templates: Seq[Template] = Seq(
    GenTupleInstances
  )

  val header = "// auto-generated boilerplate"
  val maxArity = 22

  /**
   * Return a sequence of the generated files.
   *
   * As a side-effect, it actually generates them...
   */
  def gen(dir: File): Seq[File] = templates.map { template =>
    val tgtFile = template.filename(dir)
    IO.write(tgtFile, template.body)
    tgtFile
  }

  class TemplateVals(val arity: Int) {
    val synTypes = (0 until arity).map(n => s"A$n")
    val synVals = (0 until arity).map(n => s"a$n")
    val `A..N` = synTypes.mkString(", ")
    val `a..n` = synVals.mkString(", ")
    val `(A..N)` = if (arity == 1) "Tuple1[A0]" else synTypes.mkString("(", ", ", ")")
    val `(a..n)` = if (arity == 1) "Tuple1(a)" else synVals.mkString("(", ", ", ")")
  }

  /**
   * Blocks in the templates below use a custom interpolator, combined with post-processing to
   * produce the body.
   *
   * - The contents of the `header` val is output first
   * - Then the first block of lines beginning with '|'
   * - Then the block of lines beginning with '-' is replicated once for each arity,
   *   with the `templateVals` already pre-populated with relevant relevant vals for that arity
   * - Then the last block of lines prefixed with '|'
   *
   * The block otherwise behaves as a standard interpolated string with regards to variable
   * substitution.
   */
  trait Template {
    def filename(root: File): File
    def content(tv: TemplateVals): String
    def range: IndexedSeq[Int] = 1 to maxArity
    def body: String = {
      val headerLines = header.split('\n')
      val raw = range.map(n => content(new TemplateVals(n)).split('\n').filterNot(_.isEmpty))
      val preBody = raw.head.takeWhile(_.startsWith("|")).map(_.tail)
      val instances = raw.flatMap(_.filter(_.startsWith("-")).map(_.tail))
      val postBody = raw.head.dropWhile(_.startsWith("|")).dropWhile(_.startsWith("-")).map(_.tail)
      (headerLines ++ preBody ++ instances ++ postBody).mkString("\n")
    }
  }

  object GenTupleInstances extends Template {
    override def range: IndexedSeq[Int] = 1 to maxArity

    def filename(root: File): File = root / "algebra" / "instances" / "TupleAlgebra.scala"

    def content(tv: TemplateVals): String = {
      import tv.*

      def constraints(constraint: String) =
        synTypes.map(tpe => s"$tpe: $constraint[$tpe]").mkString(", ")

      def tuple(results: TraversableOnce[String]) = {
        val resultsVec = results.toVector
        val a = synTypes.size
        val r = s"${0.until(a).map(i => resultsVec(i)).mkString(", ")}"
        if (a == 1) "Tuple1(" ++ r ++ ")"
        else s"($r)"
      }

      def binMethod(name: String) =
        synTypes.zipWithIndex.iterator.map { case (tpe, i) =>
          val j = i + 1
          s"$tpe.$name(x._$j, y._$j)"
        }

      def binTuple(name: String) =
        tuple(binMethod(name))

      def unaryTuple(name: String) = {
        val m = synTypes.zipWithIndex.map { case (tpe, i) => s"$tpe.$name(x._${i + 1})" }
        tuple(m)
      }

      def nullaryTuple(name: String) = {
        val m = synTypes.map(tpe => s"$tpe.$name")
        tuple(m)
      }

      block"""
        |package algebra
        |package instances
        |
        |import algebra.ring.{Rig, Ring, Rng, Semiring}
        |
        |trait TupleInstances extends cats.kernel.instances.TupleInstances {
        -
        -  implicit def tuple${arity}Rig[${`A..N`}](implicit ${constraints("Rig")}): Rig[${`(A..N)`}] =
        -    new Rig[${`(A..N)`}] {
        -      def zero = ${nullaryTuple("zero")}
        -      def one = ${nullaryTuple("one")}
        -      def plus(x: ${`(A..N)`}, y: ${`(A..N)`}) = ${binTuple("plus")}
        -      def times(x: ${`(A..N)`}, y: ${`(A..N)`}) = ${binTuple("times")}
        -    }
        -
        -  implicit def tuple${arity}Ring[${`A..N`}](implicit ${constraints("Ring")}): Ring[${`(A..N)`}] =
        -    new Ring[${`(A..N)`}] {
        -      def zero = ${nullaryTuple("zero")}
        -      def one = ${nullaryTuple("one")}
        -      def negate(x: ${`(A..N)`}) = ${unaryTuple("negate")}
        -      def plus(x: ${`(A..N)`}, y: ${`(A..N)`}) = ${binTuple("plus")}
        -      def times(x: ${`(A..N)`}, y: ${`(A..N)`}) = ${binTuple("times")}
        -    }
        -
        -  implicit def tuple${arity}Rng[${`A..N`}](implicit ${constraints("Rng")}): Rng[${`(A..N)`}] =
        -    new Rng[${`(A..N)`}] {
        -      def zero = ${nullaryTuple("zero")}
        -      def negate(x: ${`(A..N)`}) = ${unaryTuple("negate")}
        -      def plus(x: ${`(A..N)`}, y: ${`(A..N)`}) = ${binTuple("plus")}
        -      def times(x: ${`(A..N)`}, y: ${`(A..N)`}) = ${binTuple("times")}
        -    }
        -
        -  implicit def tuple${arity}Semiring[${`A..N`}](implicit ${constraints("Semiring")}): Semiring[${`(A..N)`}] =
        -    new Semiring[${`(A..N)`}] {
        -      def zero = ${nullaryTuple("zero")}
        -      def plus(x: ${`(A..N)`}, y: ${`(A..N)`}) = ${binTuple("plus")}
        -      def times(x: ${`(A..N)`}, y: ${`(A..N)`}) = ${binTuple("times")}
        -    }
        |}
      """
    }
  }
}
