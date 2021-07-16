
package cats.kernel.derivation

import cats.kernel.Order
import scala.deriving._
import scala.compiletime._


private[kernel] trait OrderDerivation:
  inline def combSum[Cur](inline tg: Int, inline x: Any, inline y: Any, n: Int): Int =
      inline erasedValue[Cur] match
        case _: EmptyTuple => 0
        case _: (tpe *: rest) =>
          if(tg == n) derived[tpe](using summonInline[Mirror.Of[tpe]]).asInstanceOf[Order[Any]].compare(x, y) 
          else combSum[rest](tg, x, y, n + 1)

  inline def combProduct[Cur](inline x: Product, inline y: Product, n: Int): Int =
    inline erasedValue[Cur] match
      case _: EmptyTuple => 0 
      case _: (tpe *: rest) =>
        val interm = summonInline[Order[tpe]].asInstanceOf[Order[Any]].compare(
          x.productElement(n),
          y.productElement(n)
        )

        if(interm == 0) combProduct[rest](x, y, n + 1) else interm
  
  inline def deriveSum[T](s: Mirror.SumOf[T]): Order[T] =
    new Order[T]:
      def compare(x: T, y: T): Int =
        val ordx = s.ordinal(x)
        val ordy = s.ordinal(y)
        if(ordx > ordy) 1
        else if (ordx < ordy) -1
        else combSum[s.MirroredElemTypes](ordx, x, y, 0)
  
  inline def deriveProduct[T](p: Mirror.ProductOf[T]): Order[T] = 
    new Order[T]:
      def compare(x: T, y: T): Int =

        combProduct[p.MirroredElemTypes](x.asInstanceOf[Product], y.asInstanceOf[Product], 0)
        
  inline def derived[T](using m: Mirror.Of[T]): Order[T] =
      inline m match
         case s: Mirror.SumOf[T]     => deriveSum(s)
         case p: Mirror.ProductOf[T] => deriveProduct(p)

