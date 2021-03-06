
package cats.kernel.derivation

import cats.kernel.Eq
import scala.deriving._

import scala.compiletime.{erasedValue, summonInline, summonAll}

private[kernel] trait EqDerivation:
  inline def combSum[Cur](inline tg: Int, inline x: Any, inline y: Any, n: Int): Boolean = 
    inline erasedValue[Cur] match 
      case _: EmptyTuple => false
      case _: (tpe *: rest) => 
        ((n == tg) && 
          derived[tpe](using summonInline[Mirror.Of[tpe]])
            .asInstanceOf[Eq[Any]].eqv(x, y)) 
        || combSum[rest](tg, x, y, n + 1)

  inline def combProduct[Cur <: Tuple](inline x: Product, inline y: Product, n: Int): Boolean = 
    inline erasedValue[Cur] match
      case _: EmptyTuple => true 
      case _: (tpe *: rest) => 
        summonInline[Eq[tpe]].asInstanceOf[Eq[Any]].eqv(
          x.productElement(n),
          y.productElement(n)
          ) && combProduct[rest](x, y, n + 1)

  inline def deriveSum[T](s: Mirror.SumOf[T]): Eq[T] = 
    new Eq[T]:
      def eqv(x: T, y: T): Boolean = 
        val ord = s.ordinal(x)
        (ord == s.ordinal(y)) && combSum[s.MirroredElemTypes](ord, x, y, 0)
      
  
  

  inline def deriveProduct[T](p: Mirror.ProductOf[T]): Eq[T] = 
    new Eq[T]:
      def eqv(x: T, y: T): Boolean = 
        combProduct[p.MirroredElemTypes](
          x.asInstanceOf[Product], 
          y.asInstanceOf[Product], 
          0
        )
        
  inline def derived[T](using m: Mirror.Of[T]): Eq[T] =
    inline m match
       case s: Mirror.SumOf[T]     => deriveSum(s)
       case p: Mirror.ProductOf[T] => deriveProduct(p)


