package cats
package free

/**
 * The free invariant functor on `F`. This is isomorphic to `F` as long as `F` itself is a
 * invariant functor. The function from `F[A]` to `InvariantCoyoneda[F,A]` exists even when
 * `F` is not an invariant functor. Implemented using a List of functions for stack-safety.
 */
sealed abstract class InvariantCoyoneda[F[_], A] extends Serializable { self =>
  import InvariantCoyoneda.{unsafeApply, Aux}

  /**
   * The pivot between `fi` and `k`, usually existential.
   */
  type Pivot

  /**
   * The underlying value.
   */
  val fi: F[Pivot]

  /**
   * The list of transformer functions, to be composed and lifted into `F` by `run`.
   */
  private[cats] val ks0: List[Any => Any]

  /**
   * The list of transformer functions, to be composed and lifted into `F` by `run`.
   */
  private[cats] val ks1: List[Any => Any]

  /**
   * The list of transformer functions composed into a single function, to be lifted into `F` by `run`.
   */
  final def k0: Pivot => A = Function.chain(ks0.reverse)(_).asInstanceOf[A]

  /**
   * The composed transformer function, to be lifted into `F` by `run`.
   */
  final def k1: A => Pivot = Function.chain(ks1)(_).asInstanceOf[Pivot]

  /**
   * Converts to `F[A]` given that `F` is a invariant functor
   */
  final def run(implicit F: Invariant[F]): F[A] = F.imap(fi)(k0)(k1)

  /**
   * Converts to `G[A]` given that `G` is a invariant functor
   */
  final def foldMap[G[_]](trans: F ~> G)(implicit G: Invariant[G]): G[A] =
    G.imap(trans(fi))(k0)(k1)

  /**
   * Simple function composition. Allows imap fusion without touching the underlying `F`.
   */
  final def imap[B](f: A => B)(g: B => A): Aux[F, B, Pivot] =
    unsafeApply(fi)(f.asInstanceOf[Any => Any] :: ks0)(g.asInstanceOf[Any => Any] :: ks1)

  /**
   * Modify the context `F` using transformation `f`.
   */
  final def mapK[G[_]](f: F ~> G): Aux[G, A, Pivot] =
    unsafeApply(f(fi))(ks0)(ks1)

}

object InvariantCoyoneda {

  /**
   * Lift the `Pivot` type member to a parameter. It is usually more convenient to use `Aux` than
   * a refinment type.
   */
  type Aux[F[_], A, B] = InvariantCoyoneda[F, A] { type Pivot = B }

  /**
   * `F[A]` converts to `InvariantCoyoneda[F,A]` for any `F`
   */
  def lift[F[_], A](fa: F[A]): InvariantCoyoneda[F, A] =
    apply(fa)(identity[A])(identity[A])

  /**
   * Like `lift(fa).imap(k0)`.
   */
  def apply[F[_], A, B](fa: F[A])(k0: A => B)(k1: B => A): Aux[F, B, A] =
    unsafeApply(fa)(k0.asInstanceOf[Any => Any] :: Nil)(k1.asInstanceOf[Any => Any] :: Nil)

  /**
   * Creates a `InvariantCoyoneda[F, A]` for any `F`, taking an `F[A]` and a list of
   * [[Invariant.imap]]ped functions to apply later
   */
  private[cats] def unsafeApply[F[_], A, B](fa: F[A])(_ks0: List[Any => Any])(_ks1: List[Any => Any]): Aux[F, B, A] =
    new InvariantCoyoneda[F, B] {
      type Pivot = A
      val ks0 = _ks0
      val ks1 = _ks1
      val fi = fa
    }

  /**
   * `InvariantCoyoneda[F, *]` provides a invariant functor for any `F`.
   */
  implicit def catsFreeInvariantFunctorForInvariantCoyoneda[F[_]]: Invariant[InvariantCoyoneda[F, *]] =
    new Invariant[InvariantCoyoneda[F, *]] {
      def imap[A, B](cfa: InvariantCoyoneda[F, A])(f: A => B)(g: B => A): InvariantCoyoneda[F, B] =
        cfa.imap(f)(g)
    }

}
