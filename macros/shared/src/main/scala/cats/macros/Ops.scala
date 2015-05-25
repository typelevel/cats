package cats
package macros

object Ops extends machinist.Ops {

  def uesc(c: Char): String = "$u%04X".format(c.toInt)

  val operatorNames: Map[String, String] =
    Map(
      ("$eq$eq$eq", "eqv"),
      ("$eq$bang$eq", "neqv"),
      ("$greater", "gt"),
      ("$greater$eq", "gteqv"),
      ("$less", "lt"),
      ("$less$eq", "lteqv"),
      ("$bar$plus$bar", "combine"),
      ("$bar$minus$bar", "remove")
    )
}
