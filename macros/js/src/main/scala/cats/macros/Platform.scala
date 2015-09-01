package cats.macros

import scala.reflect.macros.Context

object Platform {

  final def isJvm: Boolean = macro isJvmImpl

  final def isJs: Boolean = macro isJsImpl

  def isJvmImpl(c: Context): c.Expr[Boolean] = {
    import c.universe._
    c.Expr(Literal(Constant(false)))
  }

  def isJsImpl(c: Context): c.Expr[Boolean] = {
    import c.universe._
    c.Expr(Literal(Constant(true)))
  }
}
