import sbt.*

import Boilerplate.*
import Boilerplate.{Template, TemplateVals}
import sbt.File

object GenTupleShowInstances extends Template {
  override def range = 1 to 11
  override def filename(root: sbt.File): File =
    root / "cats" / "instances" / "NTupleShowInstances.scala"

  override def content(tv: TemplateVals): String = {
    import tv.*

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
    -    Show.show(f => $showMethod)
    |}"""
  }
}
