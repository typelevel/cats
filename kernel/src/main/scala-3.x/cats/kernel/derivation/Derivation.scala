package cats.kernel.derivation

import scala.compiletime.{erasedValue, summonInline}

inline private[derivation] def summonAll[T <: Tuple, F[_]]: List[F[Any]] =
 inline erasedValue[T] match
    case _: EmptyTuple => Nil
    case _: (t *: ts) => summonInline[F[t]].asInstanceOf[F[Any]] :: summonAll[ts, F]

inline private[derivation] def valuesIterator[T](p: T) = p.asInstanceOf[Product].productIterator

