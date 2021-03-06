
package cats.kernel.derivation

import cats.kernel.Monoid
import scala.compiletime.{erasedValue, summonInline}
import scala.deriving._


private[kernel] trait MonoidDerivation extends SemigroupDerivationAux :
  
  inline def combProduct[Cur]: Tuple = inline erasedValue[Cur] match {
    case _: EmptyTuple => EmptyTuple
    case _: (tpe *: rest) =>
      summonInline[Monoid[tpe]].empty *: combProduct[rest]
  }
  
  inline def deriveProduct[T](p: Mirror.ProductOf[T]): Monoid[T] = { 
    val semigroup = semigroupProduct[T](p)

    new Monoid[T]:
      def empty: T = p.fromProduct(combProduct[p.MirroredElemTypes]) 
      def combine(x: T, y: T): T = semigroup.combine(x, y) 
  }
        
  inline def derived[T](using m: Mirror.Of[T]): Monoid[T] =
      inline m match
         case p: Mirror.ProductOf[T] => deriveProduct(p)

