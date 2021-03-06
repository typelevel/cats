
package cats.kernel.derivation

import cats.kernel.Monoid
import scala.deriving._


private[kernel] trait MonoidDerivation extends SemigroupDerivationAux :
  private def deriveProduct[T](p: Mirror.ProductOf[T], elems: List[Monoid[_]]): Monoid[T] = { 
    val semigroup = semigroupProduct[T](p, elems)
    new Monoid[T]:
      def empty: T = 
        val arrb = new Array[Any](elems.size)

        var idx = 0
        
        elems.iterator.foreach {monoid =>
              arrb.update(idx, monoid.empty)

              idx += 1
        }
        
        p.fromProduct(Tuple.fromArray[Any](arrb))
      def combine(x: T, y: T): T = semigroup.combine(x, y) 
  }
        
  inline def derived[T](using m: Mirror.Of[T]): Monoid[T] =
      lazy val elemInstances = summonAll1[m.MirroredElemTypes, Monoid]
      inline m match
         case p: Mirror.ProductOf[T] => deriveProduct(p, elemInstances)

