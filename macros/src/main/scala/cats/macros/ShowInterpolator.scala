package cats
package macros

import scala.reflect.macros.blackbox.Context

/**
 * Adapted from https://github.com/scala/scala/blob/2.12.x/src/compiler/scala/tools/reflect/FormatInterpolator.scala
 */
object ShowInterpolator {
  def show(c: Context)(args: c.Expr[Any]*): c.Tree = {
    import c.universe._

    @inline def truly(body: => Unit): Boolean = { body ; true }
    @inline def fail(msg: String) = c.abort(c.enclosingPosition, msg)

    def interpolated(parts: List[Tree], args: List[Tree]) = {
      val catsPackage = Select(Ident(termNames.ROOTPKG), TermName("cats"))
      val doShow = Select(Select(catsPackage, TermName("Show")), TermName("doShow"))

      val args1 = args map { arg => Apply(doShow, List(arg)) }

      val scalaPackage = Select(Ident(termNames.ROOTPKG), TermName("scala"))
      val newStringContext = Select(
        New(Select(scalaPackage, TypeName("StringContext"))),
        termNames.CONSTRUCTOR
      )

      val stringContext = Apply(newStringContext, parts)

      Apply(Select(stringContext, TermName("s")), args1)
    }

    c.macroApplication match {
      case Apply(Select(Apply(_, Apply(_, parts) :: Nil), _), args) =>
        def badlyInvoked = (parts.length != args.length + 1) && truly {
          def because(s: String) = s"too $s arguments for interpolated string: expected ${parts.length-1}, but got ${args.length}"
          val (p, msg) =
            if (parts.length == 0) (c.prefix.tree.pos, "there are no parts")
            else if (args.length + 1 < parts.length)
              (if (args.isEmpty) c.enclosingPosition else args.last.pos, because("few"))
            else (args(parts.length-1).pos, because("many"))
          c.abort(p, msg)
        }
        if (badlyInvoked) c.macroApplication else interpolated(parts, args)
      case other =>
        fail(s"Unexpected application ${showRaw(other)}")
    }
  }
}
