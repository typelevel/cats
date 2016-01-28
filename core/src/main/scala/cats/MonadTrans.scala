package cats

/**
 * A typeclass which abstracts over monad transformers, providing the
 * ability to lift a monad, into the the monad transformer for another
 * monad.
 */
trait MonadTrans[MT[_[_], _]] {
  /**
   * Lift a value of type M[A] into a monad transformer MT[M, A]
   */
  def liftM[M[_]: Monad, A](ma: M[A]): MT[M, A]

  /**
   * Lift a value of type M[A] into a monad transformer MT[M, A] using
   * Unapply, this will be useful when the M[A] monad is actually not
   * in the * -> * shape. For example Xor[E,A].
   */
  def liftMU[MA](ma: MA)(implicit U: Unapply[Monad, MA]): MT[U.M, U.A] = {
    liftM[U.M, U.A](U.subst(ma))(U.TC)
  }
}

object MonadTrans {
  def apply[MT[_[_], _]](implicit MT: MonadTrans[MT]) = MT
}
