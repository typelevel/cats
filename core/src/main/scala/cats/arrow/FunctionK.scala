package cats
package arrow

import cats.data.Coproduct

import cats.macros.MacroCompat

trait FunctionK[F[_], G[_]] extends Serializable { self =>
  def apply[A](fa: F[A]): G[A]

  def compose[E[_]](f: FunctionK[E, F]): FunctionK[E, G] =
    new FunctionK[E, G] {
      def apply[A](fa: E[A]): G[A] = self.apply(f(fa))
    }

  def andThen[H[_]](f: FunctionK[G, H]): FunctionK[F, H] =
    f.compose(self)

  def or[H[_]](h: FunctionK[H, G]): FunctionK[Coproduct[F, H, ?], G] =
    new FunctionK[Coproduct[F, H, ?], G] {
      def apply[A](fa: Coproduct[F, H, A]): G[A] = fa.run match {
        case Left(ff) => self(ff)
        case Right(gg) => h(gg)
      }
    }
}

object FunctionK {

  def id[F[_]]: FunctionK[F, F] =
    new FunctionK[F, F] {
      def apply[A](fa: F[A]): F[A] = fa
    }

  def lift[F[_], G[_]](f: (F[α] ⇒ G[α]) forSome { type α }): FunctionK[F, G] =
    macro FunctionKMacros.lift[F, G]

}

object FunctionKMacros extends MacroCompat {

  def lift[F[_], G[_]](c: Context)(
    f: c.Expr[(F[α] ⇒ G[α]) forSome { type α }]
  )(
    implicit evF: c.WeakTypeTag[F[_]], evG: c.WeakTypeTag[G[_]]
  ): c.Expr[FunctionK[F, G]] = {
    import c.universe._

    def unblock(tree: Tree): Tree = tree match {
      case Block(Nil, expr) ⇒ expr
      case _                ⇒ tree
    }

    def punchHole(tpe: Type): Tree = tpe match {
      case PolyType(undet :: Nil, underlying: TypeRef) ⇒
        val α = compatNewTypeName(c, "α")
        def rebind(typeRef: TypeRef): Tree =
          if (typeRef.sym == undet) tq"$α"
          else {
            val args = typeRef.args.map {
              case ref: TypeRef => rebind(ref)
              case arg => tq"$arg"
            }
            tq"${typeRef.sym}[..$args]"
          }
        val rebound = rebind(underlying)
        tq"""({type λ[$α] = $rebound})#λ"""
      case TypeRef(pre, sym, Nil) ⇒
        tq"$sym"
      case _ =>
        c.abort(c.enclosingPosition, s"Unexpected type $tpe when lifting to FunctionK")
    }

    val tree = unblock(f.tree) match {
      case q"""($param) => $trans[..$typeArgs](${ arg: Ident })""" if param.name == arg.name ⇒

        typeArgs
          .collect { case tt: TypeTree => tt }
          .find(_.original != null)
          .foreach { param => c.abort(param.pos,
            s"type parameter $param must not be supplied when lifting function $trans to FunctionK")
          }

        val F = punchHole(evF.tpe)
        val G = punchHole(evG.tpe)

        q"""
          new FunctionK[$F, $G] {
            def apply[A](fa: $F[A]): $G[A] = $trans(fa)
          }
         """
      case other ⇒
        c.abort(other.pos, s"Unexpected tree $other when lifting to FunctionK")
    }

    c.Expr[FunctionK[F, G]](tree)
  }

}
