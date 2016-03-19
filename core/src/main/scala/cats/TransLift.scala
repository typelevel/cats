package cats


/**
 * A typeclass which abstracts over the ability to lift an M[A] into a
 * MonadTransformer
 */
trait TransLift[MT[_[_], _]] {

  /**
   * The typeclass which constrains liftT as a function of the type
   * constructor it is given. A safe "identity" value for this type
   * if your transformer does not constrain its lifted effects would
   * be `type TC[M[_]] = Unit =:= Unit`.  A more common constraint
   * might be `type TC[M[_]] = Monad[M]`.
   */
  type TC[M[_]]

  /**
   * Lift a value of type M[A] into a monad transformer MT[M, A]
   */
  def liftT[M[_]: TC, A](ma: M[A]): MT[M, A]
}

object TransLift {
  type Aux[MT[_[_], _], TC0[_[_]]] = TransLift[MT] { type TC[M[_]] = TC0[M] }
  type AuxId[MT[_[_], _]] = Aux[MT, λ[X[_] => Unit =:= Unit]]   // TODO we need a Trivial typeclass
}
