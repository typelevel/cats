package cats.syntax

import cats.data.{Ior, IorNel}

trait IorSyntax {
  implicit def catsSyntaxIorId[A](a: A): IorIdOps[A] = new IorIdOps(a)
  implicit def catsSyntaxListIorNel[A, B](list: List[IorNel[A, B]]): IorNelListOps[A, B] =
    new IorNelListOps(list)
}

final class IorIdOps[A](val a: A) extends AnyVal {
  /**
    * Wrap a value in `Ior.Right`.
    */
  def rightIor[B]: Ior[B, A] = Ior.right(a)

  /**
    * Wrap a value in `Ior.Left`.
    */
  def leftIor[B]: Ior[A, B] = Ior.left(a)

  /**
    * Wrap a value in the right side of `Ior.Both`.
    */
  def putRightIor[B](left: B): Ior[B, A] = Ior.both(left, a)

  /**
    * Wrap a value in the left side of `Ior.Both`.
    */
  def putLeftIor[B](right: B): Ior[A, B] = Ior.both(a, right)

  /**
    * Wrap a value in a NonEmptyList in `Ior.Right`.
    */
  def rightNelIor[B]: IorNel[B, A] = Ior.rightNel(a)

  /**
    * Wrap a value in a NonEmptyList in `Ior.Left`.
    */
  def leftNelIor[B]: IorNel[A, B] = Ior.leftNel(a)

  /**
    * Wrap a value in a NonEmptyList in the right side of `Ior.Both`.
    */
  def putRightNelIor[B](left: B): IorNel[B, A] = Ior.bothNel(left, a)

  /**
    * Wrap a value in a NonEmptyList in the left side of `Ior.Both`.
    */
  def putLeftNelIor[B](right: B): IorNel[A, B] = Ior.bothNel(a, right)
}


final class IorNelListOps[A, B](val list: List[IorNel[A, B]]) extends AnyVal {

  /**
    * Returns single combined IorNel by reducing a list of IorNel
    */
  def reduceToIorNel: IorNel[A, B] = list reduce (_ append _)

  def reduceToOptionIorNel: Option[IorNel[A, B]] = list reduceOption (_ append _)
}
