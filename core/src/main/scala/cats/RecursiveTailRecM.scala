package cats

import java.io.Serializable
/**
 * This is a marker type that promises that the method
 * .tailRecM for this type is stack-safe for arbitrary recursion.
 */
trait RecursiveTailRecM[F[_]] extends Serializable {
  /*
   * you can call RecusiveTailRecM[F].sameType(Monad[F]).tailRec
   * to have a static check that the types agree
   * for safer usage of tailRecM
   */
  final def sameType[M[_[_]]](m: M[F]): M[F] = m
}

object RecursiveTailRecM {
  private[this] val singleton: RecursiveTailRecM[Id] = new RecursiveTailRecM[Id] { }

  def apply[F[_]](implicit r: RecursiveTailRecM[F]): RecursiveTailRecM[F] = r

  def create[F[_]]: RecursiveTailRecM[F] =
    singleton.asInstanceOf[RecursiveTailRecM[F]]
}
