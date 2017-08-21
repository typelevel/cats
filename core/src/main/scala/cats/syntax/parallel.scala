package cats.syntax

import cats.{Monad, Parallel, Traverse}

trait ParallelSyntax {
  implicit final def catsSyntaxParallelTraverse[T[_]: Traverse, A]
  (ta: T[A]): ParallelTraversableOps[T, A] = new ParallelTraversableOps[T, A] {
    override def self = ta

    override val typeClassInstance: Traverse[T] = T
  }
}




trait ParallelTraversableOps[T[_], A] {

  val typeClassInstance : cats.Traverse[T]
  def self : T[A]

  implicit val T = typeClassInstance

  def parTraverse[M[_]: Monad, F[_], B]
  (f: A => M[B])(implicit P: Parallel[M, F]): M[T[B]] =
    Parallel.parTraverse(self)(f)

  def parSequence[M[_]: Monad, F[_], B](implicit ev: A <:< M[B], P: Parallel[M, F]): M[T[B]] =
    Parallel.parSequence(self.asInstanceOf[T[M[B]]])


}
