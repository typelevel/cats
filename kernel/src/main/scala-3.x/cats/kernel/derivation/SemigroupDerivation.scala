
package cats.kernel.derivation

import cats.kernel.Semigroup
import scala.deriving.*


private[kernel] trait SemigroupDerivation:
  private def deriveProduct[T](p: Mirror.ProductOf[T], elems: List[Semigroup[_]]): Semigroup[T] = 
    new Semigroup[T]:
      def combine(x: T, y: T): T = 
        val arrb = new Array[Any](elems.size)

        var idx = 0
        
        valuesIterator(x).zip(valuesIterator(y)).zip(elems.iterator).foreach {
          case ((l, r), comp) =>
              arrb.update(idx, comp.asInstanceOf[Semigroup[Any]].combine(l, r))

              idx += 1
        }
        
        p.fromProduct(Tuple.fromArray[Any](arrb))
        
  inline def derived[T](using m: Mirror.Of[T]): Semigroup[T] =
      lazy val elemInstances = summonAll[m.MirroredElemTypes, Semigroup]
      inline m match
         case p: Mirror.ProductOf[T] => deriveProduct(p, elemInstances)

