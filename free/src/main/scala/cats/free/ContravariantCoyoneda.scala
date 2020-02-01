package cats
package free

/**
 * The free contravariant functor on `F`. This is isomorphic to `F` as long as `F` itself is a
 * contravariant functor. The function from `F[A]` to `ContravariantCoyoneda[F,A]` exists even when
 * `F` is not a contravariant functor. Implemented using a List of functions for stack-safety.
 */
sealed abstract class ContravariantCoyoneda[F[_], A] extends Serializable { self =>
  import ContravariantCoyoneda.{unsafeApply, Aux}

  /** The pivot between `fi` and `k`, usually existential. */
  type Pivot

  /** The underlying value. */
  val fi: F[Pivot]

  /** The list of transformer functions, to be composed and lifted into `F` by `run`. */
  private[cats] val ks: List[Any => Any]

  /** The composed transformer function, to be lifted into `F` by `run`. */
  final def k: A => Pivot = Function.chain(ks)(_).asInstanceOf[Pivot]

  /** Converts to `F[A]` given that `F` is a contravariant functor */
  final def run(implicit F: Contravariant[F]): F[A] = F.contramap(fi)(k)

  /** Converts to `G[A]` given that `G` is a contravariant functor */
  final def foldMap[G[_]](trans: F ~> G)(implicit G: Contravariant[G]): G[A] =
    G.contramap(trans(fi))(k)

  /** Simple function composition. Allows contramap fusion without touching the underlying `F`. */
  final def contramap[B](f: B => A): Aux[F, B, Pivot] =
    unsafeApply(fi)(f.asInstanceOf[Any => Any] :: ks)

  /** Modify the context `F` using transformation `f`. */
  final def mapK[G[_]](f: F ~> G): Aux[G, A, Pivot] =
    unsafeApply(f(fi))(ks)

}

object ContravariantCoyoneda {

  /**
   * Lift the `Pivot` type member to a parameter. It is usually more convenient to use `Aux` than
   * a refinment type.
   */
  type Aux[F[_], A, B] = ContravariantCoyoneda[F, A] { type Pivot = B }

  /** `F[A]` converts to `ContravariantCoyoneda[F,A]` for any `F` */
  def lift[F[_], A](fa: F[A]): ContravariantCoyoneda[F, A] =
    apply(fa)(identity[A])

  /** Like `lift(fa).contramap(k0)`. */
  def apply[F[_], A, B](fa: F[A])(k0: B => A): Aux[F, B, A] =
    unsafeApply(fa)(k0.asInstanceOf[Any => Any] :: Nil)

  /**
   * Creates a `ContravariantCoyoneda[F, A]` for any `F`, taking an `F[A]` and a list of
   * [[Contravariant.contramap]]ped functions to apply later
   */
  private[cats] def unsafeApply[F[_], A, B](fa: F[A])(ks0: List[Any => Any]): Aux[F, B, A] =
    new ContravariantCoyoneda[F, B] {
      type Pivot = A
      val ks = ks0
      val fi = fa
    }

  /** `ContravariantCoyoneda[F, *]` provides a contravariant functor for any `F`. */
  implicit def catsFreeContravariantFunctorForContravariantCoyoneda[F[_]]: Contravariant[ContravariantCoyoneda[F, *]] =
    new Contravariant[ContravariantCoyoneda[F, *]] {
      def contramap[A, B](cfa: ContravariantCoyoneda[F, A])(f: B => A): ContravariantCoyoneda[F, B] =
        cfa.contramap(f)
    }

}
