import sbt._

import Boilerplate._
import Boilerplate.{Template, TemplateVals}
import sbt.File

object GenTupleShowInstances extends Template {
  def filename(root: sbt.File): File =
    root / "cats" / "instances" / "NTupleShowInstances.scala"

  def content(tv: TemplateVals): String = {
    import tv._

    val showMethod: String =
      synTypes.zipWithIndex.iterator
        .map { case (tpe, i) =>
          s"$${$tpe.show(f._${i + 1})}"
        }
        .mkString("s\"(", ",", ")\"")

    block"""
    |
    |package cats
    |package instances
    |
    |private[cats] trait NTupleShowInstances {
    -  implicit final def catsStdShowForTuple$arity[${`A..N`}]${`constraints A..N`("Show")}: Show[${`(A..N)`}] =
    -    new Show[${`(A..N)`}] {
    -      def show(f: ${`(A..N)`}): String = $showMethod
    -    }
    |}"""
  }
}
