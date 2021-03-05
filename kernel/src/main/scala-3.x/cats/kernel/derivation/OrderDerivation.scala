
package cats.kernel.derivation

import cats.kernel.Order
import scala.deriving.*


private[kernel] trait OrderDerivation:
  private def deriveSum[T](s: Mirror.SumOf[T], elems: => List[Order[_]]): Order[T] =
    new Order[T]:
      def compare(x: T, y: T): Int = 
        val ordx = s.ordinal(x)
        val ordy = s.ordinal(y)
        if(ordx > ordy) 1
        else if(ordx < ordy) -1
        else {
          val ords = elems.apply(ordx)
          ords.asInstanceOf[Order[Any]].compare(x, y)
        }
  
  private def deriveProduct[T](p: Mirror.ProductOf[T], elems: List[Order[_]]): Order[T] = 
    new Order[T]:
      def compare(x: T, y: T): Int =
        var res = 0
        valuesIterator(x).zip(valuesIterator(y)).zip(elems.iterator).foreach {
          case ((l, r), comp) =>
              res = comp.asInstanceOf[Order[Any]].compare(l, r)

              if(res != 0) return res
        }
        
        0
        
  inline given derived[T](using m: Mirror.Of[T]): Order[T] =
      lazy val elemInstances = summonAll[m.MirroredElemTypes, Order]
      inline m match
         case s: Mirror.SumOf[T]     => deriveSum(s, elemInstances)
         case p: Mirror.ProductOf[T] => deriveProduct(p, elemInstances)

