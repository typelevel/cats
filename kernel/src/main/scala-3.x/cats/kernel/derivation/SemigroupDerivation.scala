
package cats.kernel.derivation

import cats.kernel.Semigroup
import scala.compiletime.{summonInline, erasedValue}
import scala.deriving._


private[kernel] trait SemigroupDerivationAux:
  inline def combProduct[Cur](x: Product, y: Product, n: Int): Tuple = 
    inline erasedValue[Cur] match
      case _: EmptyTuple => EmptyTuple
      case _: (tpe *: rest) => 
        summonInline[Semigroup[tpe]].asInstanceOf[Semigroup[Any]].combine(
          x.productElement(n),
          y.productElement(n)
        ) *: combProduct[rest](x, y, n + 1)

  inline def semigroupProduct[T](p: Mirror.ProductOf[T]): Semigroup[T] = 
    new Semigroup[T]:
      def combine(x: T, y: T): T = 
        p.fromProduct(
          combProduct[p.MirroredElemTypes](
            x.asInstanceOf[Product], 
            y.asInstanceOf[Product], 
            0
          )
        )

private[kernel] trait SemigroupDerivation extends SemigroupDerivationAux:
  inline def derived[T](using m: Mirror.Of[T]): Semigroup[T] =
      inline m match
         case p: Mirror.ProductOf[T] => semigroupProduct(p)

