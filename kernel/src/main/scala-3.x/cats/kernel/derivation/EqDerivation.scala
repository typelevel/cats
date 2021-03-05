
package cats.kernel.derivation

import cats.kernel.Eq
import scala.deriving.*

import scala.compiletime.{erasedValue, summonInline, constValue}

inline private[derivation] def summonAll[T <: Tuple, F[_]]: List[F[Any]] =
 inline erasedValue[T] match
    case _: EmptyTuple => Nil
    case _: (t *: ts) => summonInline[F[t]].asInstanceOf[F[Any]] :: summonAll[ts, F]

inline private[derivation] def valuesIterator[T](p: T) = p.asInstanceOf[Product].productIterator
inline private[derivation] def namesIterator[T](p: T) = p.asInstanceOf[Product].productElementNames

private[kernel] trait EqDerivation:
  private def deriveSum[T](s: Mirror.SumOf[T], elems: => List[Eq[_]]): Eq[T] =
    new Eq[T]:
      def eqv(x: T, y: T): Boolean = 
        val ordx = s.ordinal(x)
        val ordy = s.ordinal(y)
        if(ordx != ordy) false
        else
          val eqs = elems.apply(ordx)
          eqs.asInstanceOf[Eq[Any]].eqv(x, y)
  
  private def deriveProduct[T](p: Mirror.ProductOf[T], elems: List[Eq[_]]): Eq[T] = 
    new Eq[T]:
      def eqv(x: T, y: T): Boolean =  
        valuesIterator(x)
          .zip(valuesIterator(y))
          .zip(elems.iterator).forall { case ((l, r), comp) =>
          comp.asInstanceOf[Eq[Any]].eqv(l, r)
        }
        
  inline def derived[T](using m: Mirror.Of[T]): Eq[T] =
      lazy val elemInstances = summonAll[m.MirroredElemTypes, Eq]
      inline m match
         case s: Mirror.SumOf[T]     => deriveSum(s, elemInstances)
         case p: Mirror.ProductOf[T] => deriveProduct(p, elemInstances)


