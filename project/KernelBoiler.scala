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
object KernelBoiler {
  import scala.StringContext._

  implicit final class BlockHelper(private val sc: StringContext) extends AnyVal {
    def block(args: Any*): String = {
      val interpolated = sc.standardInterpolator(treatEscapes, args)
      val rawLines = interpolated.split('\n')
      val trimmedLines = rawLines.map(_.dropWhile(_.isWhitespace))
      trimmedLines.map(_.dropWhile(_ == '|')).mkString("\n")
    }
  }

  val templates: Seq[Template] = Seq(GenTupleInstances)

  val header = "// auto-generated boilerplate"
  val maxArity = 22

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
    val `A..N` = synTypes.mkString(", ")
    val `a..n` = synVals.mkString(", ")
    val `_.._` = Seq.fill(arity)("_").mkString(", ")
    val `(A..N)` = if (arity == 1) "Tuple1[A0]" else synTypes.mkString("(", ", ", ")")
    val `(_.._)` = if (arity == 1) "Tuple1[_]" else Seq.fill(arity)("_").mkString("(", ", ", ")")
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
    def preBody: String
    def instances: Seq[InstanceDef]
    def range: IndexedSeq[Int] = 1 to maxArity
    def body: String = {
      val headerLines = header.split('\n')
      val tvs = range.map(n => new TemplateVals(n))
      (headerLines ++ Seq(preBody) ++ instances.flatMap(_.body(tvs))).mkString("\n")
    }
  }

  final case class InstanceDef(start: String, methods: TemplateVals => TemplatedBlock, end: String = "}") {
    def body(tvs: Seq[TemplateVals]): Seq[String] = Seq(start) ++ tvs.map(methods(_).content) ++ Seq(end)
  }

  abstract class TemplatedBlock(tv: TemplateVals) {
    import tv._

    def constraints(constraint: String): String =
      synTypes.map(tpe => s"$tpe: $constraint[$tpe]").mkString(", ")

    def tuple(results: TraversableOnce[String]): String = {
      val resultsVec = results.toVector
      val a = synTypes.size
      val r = s"${0.until(a).map(i => resultsVec(i)).mkString(", ")}"
      if (a == 1) "Tuple1(" ++ r ++ ")"
      else s"($r)"
    }

    def tupleNHeader = s"Tuple${synTypes.size}"

    def binMethod(name: String): Iterator[String] =
      synTypes.zipWithIndex.iterator.map { case (tpe, i) =>
        val j = i + 1
        s"$tpe.$name(x._$j, y._$j)"
      }

    def binTuple(name: String): String =
      tuple(binMethod(name))

    def unaryTuple(name: String): String = {
      val m = synTypes.zipWithIndex.map { case (tpe, i) => s"$tpe.$name(x._${i + 1})" }
      tuple(m)
    }

    def unaryMethod(name: String): Iterator[String] =
      synTypes.zipWithIndex.iterator.map { case (tpe, i) =>
        s"$tpe.$name(x._${i + 1})"
      }

    def nullaryTuple(name: String): String = {
      val m = synTypes.map(tpe => s"$tpe.$name")
      tuple(m)
    }

    def content: String
  }

  object GenTupleInstances extends Template {
    override def range: IndexedSeq[Int] = 1 to maxArity

    def filename(root: File): File = root / "cats" / "kernel" / "instances" / "TupleInstances.scala"

    val preBody: String =
      block"""
      package cats.kernel
      package instances"""

    def instances: Seq[InstanceDef] =
      Seq(
        InstanceDef(
          block"""
          trait TupleInstances extends TupleInstances1 {""",
          tv =>
            new TemplatedBlock(tv) {
              import tv._
              def content =
                block"""
                |  implicit def catsKernelStdCommutativeGroupForTuple$arity[${`A..N`}](
                |    implicit ${constraints("CommutativeGroup")}
                |  ): CommutativeGroup[${`(A..N)`}] =
                |    Semigroup.catsKernelCommutativeGroupForTuple$arity[${`A..N`}]
                |  implicit def catsKernelStdOrderForTuple$arity[${`A..N`}](
                |    implicit ${constraints("Order")}
                |  ): Order[${`(A..N)`}] =
                |    Eq.catsKernelOrderForTuple$arity[${`A..N`}]
                |  implicit def catsKernelStdBoundedSemilatticeForTuple$arity[${`A..N`}](
                |    implicit ${constraints("BoundedSemilattice")}
                |  ): BoundedSemilattice[${`(A..N)`}] =
                |    Semigroup.catsKernelBoundedSemilatticeForTuple$arity[${`A..N`}]
                |"""
            }
        ),
        InstanceDef(
          block"""
          private[instances] trait TupleInstances1 extends TupleInstances2 {""",
          tv =>
            new TemplatedBlock(tv) {
              import tv._
              def content =
                block"""
                |  implicit def catsKernelStdSemilatticeForTuple$arity[${`A..N`}](
                |    implicit ${constraints("Semilattice")}
                |  ): Semilattice[${`(A..N)`}] =
                |    Semigroup.catsKernelSemilatticeForTuple$arity[${`A..N`}]
                |  implicit def catsKernelStdCommutativeMonoidForTuple$arity[${`A..N`}](
                |    implicit ${constraints("CommutativeMonoid")}
                |  ): CommutativeMonoid[${`(A..N)`}] =
                |    Semigroup.catsKernelCommutativeMonoidForTuple$arity[${`A..N`}]
                |  implicit def catsKernelStdGroupForTuple$arity[${`A..N`}](
                |    implicit ${constraints("Group")}
                |  ): Group[${`(A..N)`}] =
                |    Semigroup.catsKernelGroupForTuple$arity[${`A..N`}]
                |  implicit def catsKernelStdHashForTuple$arity[${`A..N`}](
                |    implicit ${constraints("Hash")}
                |  ): Hash[${`(A..N)`}] =
                |    Eq.catsKernelHashForTuple$arity[${`A..N`}]
                |  implicit def catsKernelStdPartialOrderForTuple$arity[${`A..N`}](
                |    implicit ${constraints("PartialOrder")}
                |  ): PartialOrder[${`(A..N)`}] =
                |    Eq.catsKernelPartialOrderForTuple$arity[${`A..N`}]
                |"""
            }
        ),
        InstanceDef(
          block"""
          private[instances] trait TupleInstances2 extends TupleInstances3 {""",
          tv =>
            new TemplatedBlock(tv) {
              import tv._
              def content =
                block"""
                |  implicit def catsKernelStdBandForTuple$arity[${`A..N`}](
                |    implicit ${constraints("Band")}
                |  ): Band[${`(A..N)`}] =
                |    Semigroup.catsKernelBandForTuple$arity[${`A..N`}]
                |  implicit def catsKernelStdCommutativeSemigroupForTuple$arity[${`A..N`}](
                |    implicit ${constraints("CommutativeSemigroup")}
                |  ): CommutativeSemigroup[${`(A..N)`}] =
                |    Semigroup.catsKernelCommutativeSemigroupForTuple$arity[${`A..N`}]
                |  implicit def catsKernelStdMonoidForTuple$arity[${`A..N`}](
                |    implicit ${constraints("Monoid")}
                |  ): Monoid[${`(A..N)`}] =
                |    Semigroup.catsKernelMonoidForTuple$arity[${`A..N`}]
                |"""
            }
        ),
        InstanceDef(
          block"""
          private[instances] trait TupleInstances3 {""",
          tv =>
            new TemplatedBlock(tv) {
              import tv._
              def content =
                block"""
                |  implicit def catsKernelStdSemigroupForTuple$arity[${`A..N`}](
                |    implicit ${constraints("Semigroup")}
                |  ): Semigroup[${`(A..N)`}] =
                |    Semigroup.catsKernelSemigroupForTuple$arity[${`A..N`}]
                |  implicit def catsKernelStdEqForTuple$arity[${`A..N`}](
                |    implicit ${constraints("Eq")}
                |  ): Eq[${`(A..N)`}] =
                |    Eq.catsKernelEqForTuple$arity[${`A..N`}]
                |"""
            }
        ),
        InstanceDef(
          "private[kernel] trait TupleBandInstances extends TupleSemigroupInstances {",
          tv =>
            new TemplatedBlock(tv) {
              import tv._
              def content =
                block"""
                |  implicit def catsKernelBandForTuple$arity[${`A..N`}](
                |    implicit ${constraints("Band")}
                |  ): Band[${`(A..N)`}] = Band.instance { (x, y) =>
                |    ${binTuple("combine")}
                |  }"""
            }
        ),
        InstanceDef(
          "private[kernel] trait TupleBoundedSemilatticeInstances extends TupleGroupInstances {",
          tv =>
            new TemplatedBlock(tv) {
              import tv._
              def content =
                block"""
                |  implicit def catsKernelBoundedSemilatticeForTuple$arity[${`A..N`}](
                |    implicit ${constraints("BoundedSemilattice")}
                |  ): BoundedSemilattice[${`(A..N)`}] = BoundedSemilattice.instance(
                |    ${nullaryTuple("empty")},
                |    (x, y) => ${binTuple("combine")}
                |  )"""
            }
        ),
        InstanceDef(
          "private[kernel] trait TupleCommutativeGroupInstances extends TupleBoundedSemilatticeInstances {",
          tv =>
            new TemplatedBlock(tv) {
              import tv._
              def content =
                block"""
                |  implicit def catsKernelCommutativeGroupForTuple$arity[${`A..N`}](
                |    implicit ${constraints("CommutativeGroup")}
                |  ): CommutativeGroup[${`(A..N)`}] = new CommutativeGroup[${`(A..N)`}] {
                |    def empty = ${nullaryTuple("empty")}
                |    def inverse(x: ${`(A..N)`}) = ${unaryTuple("inverse")}
                |    def combine(x: ${`(A..N)`}, y: ${`(A..N)`}) = ${binTuple("combine")}
                |  }"""
            }
        ),
        InstanceDef(
          "private[kernel] trait TupleCommutativeMonoidInstances extends TupleSemilatticeInstances {",
          tv =>
            new TemplatedBlock(tv) {
              import tv._
              def content =
                block"""
                |  implicit def catsKernelCommutativeMonoidForTuple$arity[${`A..N`}](
                |    implicit ${constraints("CommutativeMonoid")}
                |  ): CommutativeMonoid[${`(A..N)`}] = CommutativeMonoid.instance(
                |    ${nullaryTuple("empty")},
                |    (x, y) => ${binTuple("combine")}
                |  )"""
            }
        ),
        InstanceDef(
          "private[kernel] trait TupleCommutativeSemigroupInstances extends TupleBandInstances {",
          tv =>
            new TemplatedBlock(tv) {
              import tv._
              def content =
                block"""
                |  implicit def catsKernelCommutativeSemigroupForTuple$arity[${`A..N`}](
                |    implicit ${constraints("CommutativeSemigroup")}
                |  ): CommutativeSemigroup[${`(A..N)`}] = CommutativeSemigroup.instance { (x, y) => 
                |    ${binTuple("combine")}
                |  }"""
            }
        ),
        InstanceDef(
          "private[kernel] trait TupleEqInstances {",
          tv =>
            new TemplatedBlock(tv) {
              import tv._
              def content =
                block"""
                |  implicit def catsKernelEqForTuple$arity[${`A..N`}](
                |    implicit ${constraints("Eq")}
                |  ): Eq[${`(A..N)`}] = Eq.instance { (x, y) =>
                |    ${binMethod("eqv").mkString(" && ")}
                |  }"""
            }
        ),
        InstanceDef(
          "private[kernel] trait TupleGroupInstances extends TupleCommutativeMonoidInstances {",
          tv =>
            new TemplatedBlock(tv) {
              import tv._
              def content =
                block"""
                |  implicit def catsKernelGroupForTuple$arity[${`A..N`}](
                |    implicit ${constraints("Group")}
                |  ): Group[${`(A..N)`}] = new Group[${`(A..N)`}] {
                |    def empty = ${nullaryTuple("empty")}
                |    def inverse(x: ${`(A..N)`}) = ${unaryTuple("inverse")}
                |    def combine(x: ${`(A..N)`}, y: ${`(A..N)`}) = ${binTuple("combine")}
                |  }"""
            }
        ),
        InstanceDef(
          "private[kernel] trait TupleHashInstances extends TupleEqInstances {",
          tv =>
            new TemplatedBlock(tv) {
              import tv._
              def content =
                block"""
                |  implicit def catsKernelHashForTuple$arity[${`A..N`}](
                |    implicit ${constraints("Hash")}
                |  ): Hash[${`(A..N)`}] = new Hash[${`(A..N)`}] {
                |    def hash(x: ${`(A..N)`}) =
                |      ${unaryMethod("hash").mkString(s"$tupleNHeader(", ", ", ")")}.hashCode()
                |    def eqv(x: ${`(A..N)`}, y: ${`(A..N)`}) =
                |      ${binMethod("eqv").mkString(" && ")}
                |  }"""
            }
        ),
        InstanceDef(
          "private[kernel] trait TupleMonoidInstances extends TupleCommutativeSemigroupInstances {",
          tv =>
            new TemplatedBlock(tv) {
              import tv._
              def content =
                block"""
                |  implicit def catsKernelMonoidForTuple$arity[${`A..N`}](
                |    implicit ${constraints("Monoid")}
                |  ): Monoid[${`(A..N)`}] = Monoid.instance(
                |    ${nullaryTuple("empty")},
                |    (x, y) => ${binTuple("combine")}
                |  )"""
            }
        ),
        InstanceDef(
          "private[kernel] trait TupleOrderInstances extends TuplePartialOrderInstances {",
          tv =>
            new TemplatedBlock(tv) {
              import tv._
              def content =
                block"""
                |  implicit def catsKernelOrderForTuple$arity[${`A..N`}](
                |    implicit ${constraints("Order")}
                |  ): Order[${`(A..N)`}] = Order.from { (x, y) =>
                |    ${binMethod("compare").mkString("Array(", ", ", ")")}.find(_ != 0).getOrElse(0)
                |  }"""
            }
        ),
        InstanceDef(
          "private[kernel] trait TuplePartialOrderInstances extends TupleHashInstances {",
          tv =>
            new TemplatedBlock(tv) {
              import tv._
              def content =
                block"""
                |  implicit def catsKernelPartialOrderForTuple$arity[${`A..N`}](
                |    implicit ${constraints("PartialOrder")}
                |  ): PartialOrder[${`(A..N)`}] = PartialOrder.from { (x, y) =>
                |    ${binMethod("partialCompare").mkString("Array(", ", ", ")")}.find(_ != 0.0).getOrElse(0.0)
                |  }"""
            }
        ),
        InstanceDef(
          "private[kernel] trait TupleSemigroupInstances {",
          tv =>
            new TemplatedBlock(tv) {
              import tv._
              def content =
                block"""
                |  implicit def catsKernelSemigroupForTuple$arity[${`A..N`}](
                |    implicit ${constraints("Semigroup")}
                |  ): Semigroup[${`(A..N)`}] = Semigroup.instance { (x, y) =>
                |    ${binTuple("combine")}
                |  }"""
            }
        ),
        InstanceDef(
          "private[kernel] trait TupleSemilatticeInstances extends TupleMonoidInstances {",
          tv =>
            new TemplatedBlock(tv) {
              import tv._
              def content =
                block"""
                |  implicit def catsKernelSemilatticeForTuple$arity[${`A..N`}](
                |    implicit ${constraints("Semilattice")}
                |  ): Semilattice[${`(A..N)`}] = Semilattice.instance { (x, y) =>
                |    ${binTuple("combine")}
                |  }"""
            }
        )
      )
  }

}
