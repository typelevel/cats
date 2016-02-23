package cats


/**
 * A typeclass which abstracts over the ability to lift an M[A] into a
 * MonadTransformer
 */
trait TransLift[MT[_[_], _], M[_]] {
  /**
   * Lift a value of type M[A] into a monad transformer MT[M, A]
   */
  def liftT[A](ma: M[A]): MT[M,A]
}
