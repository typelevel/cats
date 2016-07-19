package cats
package free

import scala.annotation.tailrec

import cats.data.Xor


sealed abstract class FreeT[S[_], M[_], A] {

  import FreeT._

  final def map[B](f: A => B)(implicit M: Applicative[M]): FreeT[S, M, B] =
    flatMap(a => pure(f(a)))

  /** Binds the given continuation to the result of this computation. */
  final def flatMap[B](f: A => FreeT[S, M, B]): FreeT[S, M, B] =
    Gosub(this, f)

  /**
   * Changes the underlying `Monad` for this `FreeT`, ie.
   * turning this `FreeT[S, M, A]` into a `FreeT[S, N, A]`.
   */
  def hoist[N[_]](mn: M ~> N): FreeT[S, N, A] =
    step match {
      case e @ Gosub(_, _) =>
        Gosub(e.a.hoist(mn), e.f.andThen(_.hoist(mn)))
      case Suspend(m) =>
        Suspend(mn(m))
    }

  /** Change the base functor `S` for a `FreeT` action. */
  def interpret[T[_]](st: S ~> T)(implicit M: Functor[M]): FreeT[T, M, A] =
    step match {
      case e @ Gosub(_, _) =>
        Gosub(e.a.interpret(st), e.f.andThen(_.interpret(st)))
      case Suspend(m) =>
        Suspend(M.map(m)(_.map(s => st(s))))
    }

  /**
    * Runs to completion, mapping the suspension with the given transformation
    * at each step and accumulating into the monad `M`.
    */
  def foldMap(f: S ~> M)(implicit M0: FlatMapRec[M], M1: Applicative[M]): M[A] = {
    def go(ft: FreeT[S, M, A]): M[FreeT[S, M, A] Xor A] =
      ft match {
        case Suspend(ma) => M0.flatMap(ma) {
          case Xor.Left(a) => M1.pure(Xor.Right(a))
          case Xor.Right(sa) => M0.map(f(sa))(Xor.right)
        }
        case g @ Gosub(_, _) => g.a match {
          case Suspend(mx) => M0.flatMap(mx) {
            case Xor.Left(x) => M1.pure(Xor.left(g.f(x)))
            case Xor.Right(sx) => M0.map(f(sx))(g.f andThen Xor.left)
          }
          case g0 @ Gosub(_, _) => M1.pure(Xor.left(g0.a.flatMap(g0.f(_).flatMap(g.f))))
        }
      }

    M0.tailRecM(this)(go)
  }

  /** Evaluates a single layer of the free monad **/
  def resume(implicit S: Functor[S], M0: FlatMapRec[M], M1: Applicative[M]): M[A Xor S[FreeT[S, M, A]]] = {
    def go(ft: FreeT[S, M, A]): M[FreeT[S, M, A] Xor (A Xor S[FreeT[S, M, A]])] =
      ft match {
        case Suspend(f) => M0.map(f)(as => Xor.right(as.map(S.map(_)(pure(_)))))
        case g1 @ Gosub(_, _) => g1.a match {
          case Suspend(m1) => M0.map(m1) {
            case Xor.Left(a) => Xor.left(g1.f(a))
            case Xor.Right(fc) => Xor.right(Xor.right(S.map(fc)(g1.f(_))))
          }
          case g2 @ Gosub(_, _) => M1.pure(Xor.left(g2.a.flatMap(g2.f(_).flatMap(g1.f))))
        }
      }

    M0.tailRecM(this)(go)
  }

  /**
    * Runs to completion, using a function that maps the resumption from `S` to a monad `M`.
    */
  def runM(interp: S[FreeT[S, M, A]] => M[FreeT[S, M, A]])(implicit S: Functor[S], M0: FlatMapRec[M], M1: Applicative[M]): M[A] = {
    def runM2(ft: FreeT[S, M, A]): M[FreeT[S, M, A] Xor A] =
      M0.flatMap(ft.resume) {
        case Xor.Left(a) => M1.pure(Xor.right(a))
        case Xor.Right(fc) => M0.map(interp(fc))(Xor.left)
      }

    M0.tailRecM(this)(runM2)
  }

  /**
   * Finds the first `M` instance, `m`, and maps it to contain the rest
   * of the computation. Since only `map` is used on `m`, its structure
   * is preserved.
   */
  @tailrec
  private[cats] final def toM(implicit M: Applicative[M]): M[FreeT[S, M, A]] =
    this match {
      case Suspend(m) => M.map(m) {
        case Xor.Left(a) => pure(a)
        case Xor.Right(s) => liftF(s)
      }
      case g1 @ Gosub(_, _) => g1.a match {
        case Suspend(m) => M.map(m) {
          case Xor.Left(a) => g1.f(a)
          case Xor.Right(s) => liftF[S, M, g1.A](s).flatMap(g1.f)
        }
        case g0 @ Gosub(_, _) => g0.a.flatMap(g0.f(_).flatMap(g1.f)).toM
      }
    }

  @tailrec
  private def step: FreeT[S, M, A] =
    this match {
      case g @ Gosub(_, _) => g.a match {
        case g0 @ Gosub(_, _) => g0.a.flatMap(a => g0.f(a).flatMap(g.f)).step
        case _ => g
      }
      case x => x
    }
}


object FreeT /* extends FreeTInstances */ {
  /** Suspend the computation with the given suspension. */
  private case class Suspend[S[_], M[_], A](a: M[A Xor S[A]]) extends FreeT[S, M, A]

  /** Call a subroutine and continue with the given function. */
  private case class Gosub[S[_], M[_], A0, B](a0: FreeT[S, M, A0], f0: A0 => FreeT[S, M, B]) extends FreeT[S, M, B] {
    type A = A0
    def a: FreeT[S, M, A] = a0
    def f: A => FreeT[S, M, B] = f0
  }

  /** Return the given value in the free monad. */
  def pure[S[_], M[_], A](value: A)(implicit M: Applicative[M]): FreeT[S, M, A] = Suspend(M.pure(Xor.left(value)))

  def suspend[S[_], M[_], A](a: M[A Xor S[FreeT[S, M, A]]])(implicit M: Applicative[M]): FreeT[S, M, A] =
    liftT(a).flatMap({
      case Xor.Left(a) => pure(a)
      case Xor.Right(s) => roll(s)
    })

  def tailRecM[S[_], M[_]: Applicative, A, B](a: A)(f: A => FreeT[S, M, A Xor B]): FreeT[S, M, B] =
    f(a).flatMap {
      case Xor.Left(a0) => tailRecM(a0)(f)
      case Xor.Right(b) => pure[S, M, B](b)
    }

  def liftT[S[_], M[_], A](value: M[A])(implicit M: Functor[M]): FreeT[S, M, A] =
    Suspend(M.map(value)(Xor.left))

  /** A version of `liftT` that infers the nested type constructor. */
  def liftTU[S[_], MA](value: MA)(implicit M: Unapply[Functor, MA]): FreeT[S, M.M, M.A] =
    liftT[S, M.M, M.A](M.subst(value))(M.TC)

  /** Suspends a value within a functor in a single step. Monadic unit for a higher-order monad. */
  def liftF[S[_], M[_], A](value: S[A])(implicit M: Applicative[M]): FreeT[S, M, A] =
    Suspend(M.pure(Xor.right(value)))

  def roll[S[_], M[_], A](value: S[FreeT[S, M, A]])(implicit M: Applicative[M]): FreeT[S, M, A] =
    liftF[S, M, FreeT[S, M, A]](value).flatMap(identity)

}

sealed abstract class FreeTInstances6 {
  /*
  implicit def freeTMonadWriter[S[_], M[_], E](implicit M1: MonadWriter[M, E]): MonadWriter[FreeT[S, M, ?], E] =
    new MonadWriter[FreeT[S, M, ?], E] with FreeTMonad[S, M] {

      override def M = implicitly

      override def writer[A](aw: (E, A)) =
        FreeT.liftT(M1.writer(aw))

      override def listen[A](fa: FreeT[S,M,A]) : FreeT[S,M,(E, A)] = {
        val tmp = M1.flatMap[(A Xor A, W), A Xor (B, W)](M1.listen(ma.)){
          case (Xor.Left(a), _) => M1.pure(Xor.left(a))
          case (Xor.Right(b), w) => M1.pure(Xor.right((b, w)))
        }

      }
        FreeT.liftT(flatMap(fa.value)(a => map(fa.written)(l => (l, (l, a)))))
    }
   */
}

 // def writer[A](aw: (W, A)): F[A]



sealed abstract class FreeTInstances5 extends FreeTInstances6 {
  implicit def freeTMonadReader[S[_], M[_], E](implicit M1: MonadReader[M, E]): MonadReader[FreeT[S, M, ?], E] =
    new MonadReader[FreeT[S, M, ?], E] with FreeTMonad[S, M] {
      override def M = implicitly
      override def ask =
        FreeT.liftT(M1.ask)
      override def local[B](f: E => E)(fa: FreeT[S, M, B]) =
        fa.hoist(new (M ~> M){
          def apply[A](a: M[A]) = M1.local(f)(a)
        })
    }
}

sealed abstract class FreeTInstances4 extends FreeTInstances5 {
  implicit def freeTMonadState[S[_], M[_], E](implicit M1: MonadState[M, E]): MonadState[FreeT[S, M, ?], E] =
    new MonadState[FreeT[S, M, ?], E] with FreeTMonad[S, M] {
      override def M = implicitly
      override def get =
        FreeT.liftT(M1.get)
      override def set(s: E) =
        FreeT.liftT(M1.set(s))
    }
}

sealed abstract class FreeTInstances3 extends FreeTInstances4 {
  implicit def freeTMonadError[S[_], M[_]: FlatMapRec, E](implicit E: MonadError[M, E]): MonadError[FreeT[S, M, ?], E] =
    new MonadError[FreeT[S, M, ?], E] with FreeTMonad[S, M] {
      override def M = implicitly
      override def handleErrorWith[A](fa: FreeT[S, M, A])(f: E => FreeT[S, M, A]) =
        FreeT.liftT[S, M, FreeT[S, M, A]](E.handleErrorWith(fa.toM)(f.andThen(_.toM)))(M).flatMap(identity)
      override def raiseError[A](e: E) =
        FreeT.liftT(E.raiseError[A](e))(M)
    }
}

sealed abstract class FreeTInstances2 extends FreeTInstances3 {
  implicit def freeTFlatMap[S[_], M[_]](implicit M0: Applicative[M]): FlatMap[FreeT[S, M, ?]] =
    new FreeTFlatMap[S, M] {
      implicit def M: Applicative[M] = M0
    }

  implicit def freeTTransLift[S[_]]: TransLift[FreeT[S, ?[_], ?]] =
    new TransLift[FreeT[S, ?[_], ?]] {

      type TC[M[_]] = Functor[M]

      override def liftT[M[_]: Functor, A](ma: M[A]): FreeT[S, M, A] =
        FreeT.liftT(ma)
    }

  implicit def freeTFoldable[S[_]: Foldable: Functor, M[_]: Foldable: Applicative: FlatMapRec]: Foldable[FreeT[S, M, ?]] =
    new FreeTFoldable[S, M] {
      override def S = implicitly
      override def F = implicitly
      override def M = implicitly
      override def M1 = implicitly
      override def M2 = implicitly
    }
}

sealed abstract class FreeTInstances1 extends FreeTInstances2 {
  implicit def freeTTraverse[S[_]: Traverse, M[_]: Traverse: Applicative: FlatMapRec]: Traverse[FreeT[S, M, ?]] =
    new FreeTTraverse[S, M] {
      override def F = implicitly
      override def M = implicitly
      override def M1 = implicitly
      override def M2 = implicitly
    }
}

sealed abstract class FreeTInstances0 extends FreeTInstances1 {
  implicit def freeTMonad[S[_], M[_]](implicit M0: Applicative[M]): Monad[FreeT[S, M, ?]] with FlatMapRec[FreeT[S, M, ?]] =
    new FreeTMonad[S, M] {
      def M = M0
    }

  implicit def freeTCombine[S[_], M[_]: Applicative: FlatMapRec: SemigroupK]: SemigroupK[FreeT[S, M, ?]] =
    new FreeTCombine[S, M] {
      override def M = implicitly
      override def M1 = implicitly
      override def M2 = implicitly
    }
}
 
sealed abstract class FreeTInstances extends FreeTInstances0 {
  implicit def freeTMonadCombine[S[_], M[_]: Alternative: FlatMapRec]: MonadCombine[FreeT[S, M, ?]] =
    new MonadCombine[FreeT[S, M, ?]] with FreeTCombine[S, M] with FreeTMonad[S, M] {
      override def M = implicitly
      override def M1 = implicitly
      override def M2 = implicitly

      override def empty[A] = FreeT.liftT[S, M, A](MonoidK[M].empty[A])(M)
    }
}
 
private trait FreeTFlatMap[S[_], M[_]] extends FlatMap[FreeT[S, M, ?]] {
  implicit def M: Applicative[M]

  override final def map[A, B](fa: FreeT[S, M, A])(f: A => B): FreeT[S, M, B] = fa.map(f)
  def flatMap[A, B](fa: FreeT[S, M, A])(f: A => FreeT[S, M, B]): FreeT[S, M, B] = fa.flatMap(f)
}
 
private trait FreeTMonad[S[_], M[_]] extends Monad[FreeT[S, M, ?]] with FlatMapRec[FreeT[S, M, ?]] with FreeTFlatMap[S, M] {
  implicit def M: Applicative[M]

  override final def pure[A](a: A) =
    FreeT.pure[S, M, A](a)
  override final def tailRecM[A, B](a: A)(f: A => FreeT[S, M, A Xor B]) =
    FreeT.tailRecM(a)(f)
}
 
private trait FreeTCombine[S[_], M[_]] extends SemigroupK[FreeT[S, M, ?]] {
  implicit def M: Applicative[M]
  implicit def M1: FlatMapRec[M]
  def M2: SemigroupK[M]
  override final def combineK[A](a: FreeT[S, M, A], b: FreeT[S, M, A]) =
    FreeT.liftT(M2.combineK(a.toM, b.toM))(M).flatMap(identity)
}

private trait FreeTFoldable[S[_], M[_]] extends Foldable[FreeT[S, M, ?]] with Foldable.FromFoldMap[FreeT[S, M, ?]] {
  implicit def S: Functor[S]
  implicit def M: Applicative[M]
  implicit def M1: FlatMapRec[M]
  def F: Foldable[S]
  def M2: Foldable[M]

  override final def foldMap[A, B: Monoid](fa: FreeT[S, M, A])(f: A => B): B =
    M2.foldMap(fa.resume){
      case Xor.Right(a) =>
        F.foldMap(a)(foldMap(_)(f))
      case Xor.Left(a) =>
        f(a)
    }
}

private trait FreeTTraverse[S[_], M[_]] extends Traverse[FreeT[S, M, ?]] with FreeTFoldable[S, M] with FreeTFlatMap[S, M] {
  override final def S: Functor[S] = F
  override implicit def F: Traverse[S]
  override def M2: Traverse[M]
  override implicit def M: Applicative[M]
  override implicit def M1: FlatMapRec[M]

  override final def traverse[G[_], A, B](fa: FreeT[S, M, A])(f: A => G[B])(implicit G: Applicative[G]) =
    G.map(
      M2.traverse(fa.resume){
        case Xor.Right(a) =>
          G.map(F.traverse(a)(traverse(_)(f)))(FreeT.roll(_)(M))
        case Xor.Left(a) =>
          G.map(f(a))(FreeT.pure[S, M, B])
      }
    )(FreeT.liftT(_)(M).flatMap(identity))
}


