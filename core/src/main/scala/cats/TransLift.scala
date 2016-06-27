package cats

/**
 * A type class which abstracts over the ability to lift an M[A] into a
 * MonadTransformer
 */
trait TransLift[MT[_[_], _]] {

  /**
   * The type class which constrains liftT as a function of the type
   * constructor it is given. A safe "identity" value for this type
   * if your transformer does not constrain its lifted effects would
   * be `type TC[M[_]] = Trivial`.  A more common constraint might be
   * `type TC[M[_]] = Monad[M]`.
   */
  type TC[M[_]]

  /**
   * Lift a value of type M[A] into a monad transformer MT[M, A]
   */
  def liftT[M[_]: TC, A](ma: M[A]): MT[M, A]
}

object TransLift {
  type Aux[MT[_[_], _], TC0[_[_]]] = TransLift[MT] { type TC[M[_]] = TC0[M] }
  type AuxId[MT[_[_], _]] = Aux[MT, Trivial.PH1]
}
