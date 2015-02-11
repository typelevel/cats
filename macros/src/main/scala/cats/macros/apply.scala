package cats.macros
package apply

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context
import scala.annotation.StaticAnnotation

object Builders {
  val Identifiers = "ABCDEFGHIJKLMNOPQRSTUVWXYZ".toList.map(_.toString) map { c => if (c == "F") { "FF"} else { c } }
  val MaxArity = 12

  def build(c: Context)(annottees: c.Expr[Any]*):c.Expr[Any] = {
    import c._; import universe._

    def builderName(arity: Int) = TypeName(s"ApplyBuilder$arity")
    def id(arity: Int) = Identifiers(arity - 1)
    def termName(arity: Int) = TermName(id(arity).toLowerCase)
    def typeName(arity: Int) = TypeName(id(arity))

    def impl(arity: Int):ClassDef = {
      val T = typeName(arity)
      val i = termName(arity)
      val name = builderName(arity)

      val (nextType, nextBuilder) = if (arity >= MaxArity) {
        (q"()", q"()")
      } else {
        val nextBuilderName = builderName(arity + 1)
        val nextBuilderTerm = termName(arity + 1)

        (
          impl(arity + 1),
          q"""
            def |@|[Z](z: F[Z]): $nextBuilderName[Z] = new $nextBuilderName[Z] {
              val $nextBuilderTerm: F[Z] = z
            }
          """
        )
      }

      val callMap = TermName(s"map$arity")
      val callApply = TermName(s"apply$arity")
      val callTuple = TermName(s"tuple$arity")

      val termArgs = (1 to arity) map { termName(_) }
      val typeArgs = (1 to arity) map { typeName(_) }

      q"""
        trait ${name}[$T] {
          val $i:F[$T]

          def apply[Z](f: F[(..$typeArgs) => Z])(implicit F: Apply[F]) = F.$callApply(..$termArgs)(f)
          def map[Z](f: (..$typeArgs) => Z)(implicit F: Apply[F]):F[Z] = F.$callMap(..$termArgs)(f)
          def tupled[Z](implicit F: Apply[F]) = F.$callTuple(..$termArgs)

          $nextBuilder
          $nextType
        }"""
    }
    c.Expr(impl(2))
  }
}

class Builders extends StaticAnnotation {
  def macroTransform(annottees: Any*):Any = macro Builders.build
}


