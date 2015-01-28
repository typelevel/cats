package cats
package free

/**
 * The dual view of the Yoneda lemma. Also a free functor on `F`.
 * This is isomorphic to `F` as long as `F` itself is a functor.
 * The homomorphism from `F[A]` to `Coyoneda[F,A]` exists even when
 * `F` is not a functor.
 */
sealed abstract class Coyoneda[F[_], A] { self =>

  /** The pivot between `fi` and `k`, usually existential. */
  type Pivot

  /** The underlying value. */
  val fi: F[Pivot]

  /** The transformer function, to be lifted into `F` by `run`. */
  val k: Pivot => A

  import Coyoneda.{Aux, apply}

  /** Converts to `F[A]` given that `F` is a functor */
  final def run(implicit F: Functor[F]): F[A] = F.map(fi)(k)

  /** Converts to `Yoneda[F,A]` given that `F` is a functor */
  final def toYoneda(implicit F: Functor[F]): Yoneda[F, A] =
    new Yoneda[F, A] {
      def apply[B](f: A => B) = F.map(fi)(k andThen f)
    }

  /**
   * Simple function composition. Allows map fusion without touching
   * the underlying `F`.
   */
  final def map[B](f: A => B): Aux[F, B, Pivot] =
    apply(fi)(f compose k)

  final def transform[G[_]](f: F ~> G): Aux[G, A, Pivot] =
    apply(f(fi))(k)

  // import Id._
  // 
  // /** `Coyoneda[F,_]` is the left Kan extension of `F` along `Id` */
  // def toLan: Lan[Id, F, A] = new Lan[Id, F, A] {
  //   type I = self.I
  //   val v = fi
  //   def f(i: I) = k(i)
  // }
}

object Coyoneda /*extends CoyonedaInstances*/ {
  /** Lift the `Pivot` type member to a parameter. It is usually more
    * convenient to use `Aux` than a structural type.
    */
  type Aux[F[_], A, B] = Coyoneda[F, A] { type Pivot = B }

  /** `F[A]` converts to `Coyoneda[F,A]` for any `F` */
  def lift[F[_], A](fa: F[A]): Coyoneda[F, A] = apply(fa)(identity[A])

  /**
   * Represents a partially-built Coyoneda instance. Used in the `by` method.
   */
  final class By[F[_]] {
    def apply[A, B](k: A => B)(implicit F: F[A]): Aux[F, B, A] = Coyoneda(F)(k)
  }

  /**
   * Partial application of type parameters to `apply`.
   * 
   * It can be nicer to say `Coyoneda.by[F]{ x: X => ... }`
   * 
   * ...instead of `Coyoneda[...](...){ x => ... }`.
   */
  def by[F[_]]: By[F] = new By[F]

  /** Like `lift(fa).map(_k)`. */
  def apply[F[_], A, B](fa: F[A])(k0: A => B): Aux[F, B, A] =
    new Coyoneda[F, B] {
      type Pivot = A
      val k = k0
      val fi = fa
    }

  // import Isomorphism._
  // 
  // def iso[F[_]: Functor]: Coyoneda[F, ?] <~> F =
  //   new IsoFunctorTemplate[Coyoneda[F, ?], F] {
  //     def from[A](fa: F[A]) = lift(fa)
  //     def to[A](fa: Coyoneda[F, A]) = fa.run
  //   }
  // 
  // /** Turns a natural transformation F ~> G into CF ~> G */
  // def liftTF[F[_], G[_]: Functor](fg: F ~> G): Coyoneda[F, ?] ~> G = {
  //   type CF[A] = Coyoneda[F, A]
  //   type CG[A] = Coyoneda[G, A]
  //   val m: (CF ~> CG) = liftT(fg)
  //   val n: (CG ~> G) = iso[G].to
  //   n compose m
  // }
  // 
  // /** Turns a natural transformation F ~> G into CF ~> CG */
  // def liftT[F[_], G[_]](fg: F ~> G): Coyoneda[F, ?] ~> Coyoneda[G, ?] =
  //   new (Coyoneda[F, ?] ~> Coyoneda[G, ?]) {
  //     def apply[A](c: Coyoneda[F, A]) = c.trans(fg)
  //   }

}

// sealed abstract class CoyonedaInstances extends CoyonedaInstances0 {
//   implicit def coyonedaOrder[A, F[_]](implicit A: Order[F[A]], F: Functor[F]): Order[Coyoneda[F, A]] =
//     new IsomorphismOrder[Coyoneda[F, A], F[A]] {
//       def G = A
//       def iso = Coyoneda.iso[F].run
//     }
// }
// 
// sealed abstract class CoyonedaInstances0 extends CoyonedaInstances1 {
//   implicit def coyonedaComonad[F[_]: Comonad]: Comonad[Coyoneda[F, ?]] =
//     new IsomorphismComonad[Coyoneda[F, ?], F] {
//       def G = implicitly
//       def iso = Coyoneda.iso
//     }
// }
// 
// sealed abstract class CoyonedaInstances1 extends CoyonedaInstances2 {
//   implicit def coyonedaEqual[A, F[_]](implicit A: Equal[F[A]], F: Functor[F]): Equal[Coyoneda[F, A]] =
//     new IsomorphismEqual[Coyoneda[F, A], F[A]] {
//       def G = A
//       def iso = Coyoneda.iso[F].run
//     }
// 
//   implicit def coyonedaCobind[F[_]: Cobind]: Cobind[Coyoneda[F, ?]] =
//     new IsomorphismCobind[Coyoneda[F, ?], F] {
//       def G = implicitly
//       def iso = Coyoneda.iso
//     }
// }
// 
// sealed abstract class CoyonedaInstances2 extends CoyonedaInstances3 {
//   implicit def coyonedaTraverse1[F[_]: Traverse1]: Traverse1[Coyoneda[F, ?]] =
//     new IsomorphismTraverse1[Coyoneda[F, ?], F] {
//       def G = implicitly
//       def iso = Coyoneda.iso
//     }
// }
// 
// sealed abstract class CoyonedaInstances3 extends CoyonedaInstances4 {
//   implicit def coyonedaMonadPlus[F[_]: MonadPlus]: MonadPlus[Coyoneda[F, ?]] =
//     new IsomorphismMonadPlus[Coyoneda[F, ?], F] {
//       def G = implicitly
//       def iso = Coyoneda.iso
//     }
// }
// 
// sealed abstract class CoyonedaInstances4 extends CoyonedaInstances5 {
//   implicit def coyonedaApplicativePlus[F[_]: ApplicativePlus]: ApplicativePlus[Coyoneda[F, ?]] =
//     new IsomorphismApplicativePlus[Coyoneda[F, ?], F] {
//       def G = implicitly
//       def iso = Coyoneda.iso
//     }
// }
// 
// sealed abstract class CoyonedaInstances5 extends CoyonedaInstances6 {
//   implicit def coyonedaMonad[F[_]: Monad]: Monad[Coyoneda[F, ?]] =
//     new IsomorphismMonad[Coyoneda[F, ?], F] {
//       def G = implicitly
//       def iso = Coyoneda.iso
//     }
// 
//   implicit def coyonedaPlusEmpty[F[_]: PlusEmpty: Functor]: PlusEmpty[Coyoneda[F, ?]] =
//     new IsomorphismEmpty[Coyoneda[F, ?], F] {
//       def G = implicitly
//       def iso = Coyoneda.iso
//     }
// }
// 
// sealed abstract class CoyonedaInstances6 extends CoyonedaInstances7 {
//   implicit def coyonedaBind[F[_]: Bind]: Bind[Coyoneda[F, ?]] =
//     new IsomorphismBind[Coyoneda[F, ?], F] {
//       def G = implicitly
//       def iso = Coyoneda.iso
//     }
// 
//   implicit def coyonedaPlus[F[_]: Plus: Functor]: Plus[Coyoneda[F, ?]] =
//     new IsomorphismPlus[Coyoneda[F, ?], F] {
//       def G = implicitly
//       def iso = Coyoneda.iso
//     }
// }
// 
// sealed abstract class CoyonedaInstances7 extends CoyonedaInstances8 {
//   implicit def coyonedaApplicative[F[_]: Applicative]: Applicative[Coyoneda[F, ?]] =
//     new IsomorphismApplicative[Coyoneda[F, ?], F] {
//       def G = implicitly
//       def iso = Coyoneda.iso
//     }
// }
// 
// sealed abstract class CoyonedaInstances8 extends CoyonedaInstances9 {
//   implicit def coyonedaFoldable1[F[_]: Foldable1]: Foldable1[Coyoneda[F, ?]] =
//     new CoyonedaFoldable1[F]{ def F = implicitly }
// 
//   implicit def coyonedaApply[F[_]: Apply]: Apply[Coyoneda[F, ?]] =
//     new IsomorphismApply[Coyoneda[F, ?], F] {
//       def G = implicitly
//       def iso = Coyoneda.iso
//     }
// }
// 
// sealed abstract class CoyonedaInstances9 extends CoyonedaInstances10 {
//   implicit def coyonedaTraverse[F[_]: Traverse]: Traverse[Coyoneda[F, ?]] =
//     new IsomorphismTraverse[Coyoneda[F, ?], F] {
//       def G = implicitly
//       def iso = Coyoneda.iso
//     }
// 
//   implicit def coyonedaContravariant[F[_]: Contravariant: Functor]: Contravariant[Coyoneda[F, ?]] =
//     new IsomorphismContravariant[Coyoneda[F, ?], F] {
//       def G = implicitly
//       def iso = Coyoneda.iso
//     }
// }
// 
// sealed abstract class CoyonedaInstances10 {
//   implicit def coyonedaFoldable[F[_]: Foldable]: Foldable[Coyoneda[F, ?]] =
//     new CoyonedaFoldable[F]{ def F = implicitly }
// 
//   /** `Coyoneda[F,_]` is a functor for any `F` */
//   implicit def coyonedaFunctor[F[_]]: Functor[Coyoneda[F, ?]] =
//     new Functor[Coyoneda[F, ?]] {
//       def map[A, B](ya: Coyoneda[F, A])(f: A => B) = ya map f
//     }
// }

// private trait CoyonedaFoldable[F[_]] extends Foldable[Coyoneda[F, ?]] {
//   def F: Foldable[F]
// 
//   override final def foldMap[A, B: Monoid](fa: Coyoneda[F, A])(f: A => B) =
//     F.foldMap(fa.fi)(i => f(fa.k(i)))
//   override final def foldRight[A, B](fa: Coyoneda[F, A], z: => B)(f: (A, => B) => B) =
//     F.foldRight(fa.fi, z)((i, b) => f(fa.k(i), b))
//   override final def foldLeft[A, B](fa: Coyoneda[F, A], z: B)(f: (B, A) => B) =
//     F.foldLeft(fa.fi, z)((b, i) => f(b, fa.k(i)))
// }
// 
// private abstract class CoyonedaFoldable1[F[_]] extends Foldable1[Coyoneda[F, ?]] with CoyonedaFoldable[F] {
//   def F: Foldable1[F]
// 
//   override final def foldMap1[A, B: Semigroup](fa: Coyoneda[F, A])(f: A => B) =
//     F.foldMap1(fa.fi)(i => f(fa.k(i)))
//   override final def foldMapRight1[A, B](fa: Coyoneda[F, A])(z: A => B)(f: (A, => B) => B) =
//     F.foldMapRight1(fa.fi)(i => z(fa.k(i)))((i, b) => f(fa.k(i), b))
// }

