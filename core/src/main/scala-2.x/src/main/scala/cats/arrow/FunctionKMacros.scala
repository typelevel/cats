package cats
package arrow

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

private[arrow] class FunctionKMacroMethods {
  protected type τ[F[_], G[_]]

  /**
   * Lifts function `f` of `F[A] => G[A]` into a `FunctionK[F, G]`.
   *
   * {{{
   *   def headOption[A](list: List[A]): Option[A] = list.headOption
   *   val lifted: FunctionK[List, Option] = FunctionK.lift(headOption)
   * }}}
   *
   * Note: This method has a macro implementation that returns a new
   * `FunctionK` instance as follows:
   *
   * {{{
   *   new FunctionK[F, G] {
   *     def apply[A](fa: F[A]): G[A] = f(fa)
   *   }
   * }}}
   *
   * Additionally, the type parameters on `f` must not be specified.
   */
  def lift[F[_], G[_]](f: (F[α] => G[α]) forSome { type α }): FunctionK[F, G] =
    macro FunctionKMacros.lift[F, G]

  /**
   * Lifts function `f` of `F[A] => G[A]` into a `FunctionK[F, G]`.
   *
   * {{{
   *   def headOption[A](list: List[A]): Option[A] = list.headOption
   *   val lifted = FunctionK.liftFunction[List, Option](headOption)
   * }}}
   *
   * Note: The weird `τ[F, G]` parameter is there to compensate for
   * the lack of polymorphic function types in Scala 2.
   */
  def liftFunction[F[_], G[_]](f: F[τ[F, G]] => G[τ[F, G]]): FunctionK[F, G] =
    new FunctionK[F, G] {
      def apply[A](fa: F[A]): G[A] = f.asInstanceOf[F[A] => G[A]](fa)
    }
}

private[arrow] object FunctionKMacros {

  def lift[F[_], G[_]](c: blackbox.Context)(
    f: c.Expr[(F[α] => G[α]) forSome { type α }]
  )(implicit evF: c.WeakTypeTag[F[Any]], evG: c.WeakTypeTag[G[Any]]): c.Expr[FunctionK[F, G]] =
    c.Expr[FunctionK[F, G]](new Lifter[c.type](c).lift[F, G](f.tree))

  private class Lifter[C <: blackbox.Context](val c: C) {
    import c.universe._

    def lift[F[_], G[_]](tree: Tree)(implicit evF: c.WeakTypeTag[F[Any]], evG: c.WeakTypeTag[G[Any]]): Tree = {
      def liftFunction(function: Tree): Tree =
        function match {
          case q"($param) => $trans[..$typeArgs]($arg)" if param.symbol == arg.symbol =>
            for (typeArg @ TypeTree() <- typeArgs) if (typeArg.original != null) {
              c.abort(
                typeArg.pos,
                s"type parameter $typeArg must not be supplied when lifting function $trans to FunctionK"
              )
            }

            val F = typeConstructorOf[F[Any]]
            val G = typeConstructorOf[G[Any]]
            q"${reify(FunctionK)}.liftFunction[$F, $G]($trans(_))"

          case other =>
            c.abort(other.pos, s"Unexpected tree $other when lifting to FunctionK")
        }

      tree match {
        case Block(Nil, expr)   => liftFunction(expr)
        case Block(stats, expr) => Block(stats, liftFunction(expr))
        case other              => liftFunction(other)
      }
    }

    private def typeConstructorOf[A: WeakTypeTag]: Type =
      weakTypeOf[A].typeConstructor.etaExpand
  }
}
