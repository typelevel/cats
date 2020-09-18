import sbt._

/**
 * Generate a range of boilerplate classes that would be tedious to write and maintain by hand.
 *
 * Copied, with some modifications, from
 * [[https://github.com/milessabin/shapeless/blob/master/project/Boilerplate.scala Shapeless]].
 *
 * @author Miles Sabin
 * @author Kevin Wright
 */

trait BoilerPlateHelper {
  import scala.StringContext._

  implicit class BlockHelper(private val sc: StringContext) {
    def block(args: Any*): String = {
      val interpolated = sc.standardInterpolator(treatEscapes, args)
      val rawLines = interpolated.split('\n')
      val trimmedLines = rawLines.map(_.dropWhile(_.isWhitespace))
      trimmedLines.mkString("\n")
    }
  }

  val templates: Seq[Template]
  val header: String
  val maxArity: Int = 22

  trait Template {
    def filename(root: File): File
    def range: Seq[Int] = 1 to maxArity
    def body: String
  }

  trait DefaultTemplate extends Template {
    def filename(root: File): File
    def content(tv: TemplateVals): String
    def body: String = {
      def expandInstances(contents: Seq[Array[String]], acc: Array[String] = Array.empty): Array[String] =
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

  /**
   * Return a sequence of the generated files.
   *
   * As a side-effect, it actually generates them...
   */
  def gen(dir: File): Seq[File] =
    templates.map { template =>
      val tgtFile = template.filename(dir)
      IO.write(tgtFile, template.body)
      tgtFile
    }

  class TemplateVals(val arity: Int) {
    val synTypes = (0 until arity).map(n => s"A$n")
    val synVals = (0 until arity).map(n => s"a$n")
    val synTypedVals = synVals.zip(synTypes).map { case (v, t) => v + ":" + t }
    val `A..N` = synTypes.mkString(", ")
    val `a..n` = synVals.mkString(", ")
    val `_.._` = Seq.fill(arity)("_").mkString(", ")
    val `(A..N)` = if (arity == 1) "Tuple1[A0]" else synTypes.mkString("(", ", ", ")")
    val `(_.._)` = if (arity == 1) "Tuple1[_]" else Seq.fill(arity)("_").mkString("(", ", ", ")")
    val `(a..n)` = if (arity == 1) "Tuple1(a)" else synVals.mkString("(", ", ", ")")
    val `a:A..n:N` = synTypedVals.mkString(", ")
  }
}
