package cats
package macros

import scala.reflect.NameTransformer

object Ops extends machinist.Ops {

  def uesc(c: Char): String = "$u%04X".format(c.toInt)

  val operatorNames: Map[String, String] =
    List(
      ("===", "eqv"),
      ("=!=", "neqv"),
      (">", "gt"),
      (">=", "gteqv"),
      ("<", "lt"),
      ("<=", "lteqv"),
      ("|+|", "combine"),
      ("|-|", "remove")
    ).map{ case (k, v) =>
      (NameTransformer.encode(k), v)
    }.toMap
}
