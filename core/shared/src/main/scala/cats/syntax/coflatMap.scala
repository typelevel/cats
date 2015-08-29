package cats
package syntax

trait CoflatMapSyntax1 {
  implicit def coflatMapSyntaxU[FA](fa: FA)(implicit U: Unapply[CoflatMap, FA]): CoflatMap.Ops[U.M, U.A] = new CoflatMap.Ops[U.M, U.A] {
    val self = U.subst(fa)
    val typeClassInstance = U.TC
  }
}

trait CoflatMapSyntax extends CoflatMap.ToCoflatMapOps with CoflatMapSyntax1
