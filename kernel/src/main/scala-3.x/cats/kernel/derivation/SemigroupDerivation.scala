
package cats.kernel.derivation

import cats.kernel.Semigroup
import scala.deriving._


private[kernel] trait SemigroupDerivationAux:
  protected def semigroupProduct[T](p: Mirror.ProductOf[T], elems: List[Semigroup[_]]): Semigroup[T] = 
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

private[kernel] trait SemigroupDerivation extends SemigroupDerivationAux:
  inline def derived[T](using m: Mirror.Of[T]): Semigroup[T] =
      lazy val elemInstances = summonAll1[m.MirroredElemTypes, Semigroup]
      inline m match
         case p: Mirror.ProductOf[T] => semigroupProduct(p, elemInstances)

