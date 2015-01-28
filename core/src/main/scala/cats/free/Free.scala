package cats
package free

import scala.annotation.tailrec

object Free {

  /** Return from the computation with the given value. */
  case class Pure[S[_], A](a: A) extends Free[S, A]

  /** Suspend the computation with the given suspension. */
  case class Suspend[S[_], A](a: S[Free[S, A]]) extends Free[S, A]

  /** Call a subroutine and continue with the given function. */
  sealed abstract class Gosub[S[_], B] extends Free[S, B] {
    type C
    val a: () => Free[S, C]
    val f: C => Free[S, B]
  }

  def gosub[S[_], A, B](a0: () => Free[S, A])(f0: A => Free[S, B]): Free[S, B] =
    new Gosub[S, B] {
      type C = A
      val a = a0
      val f = f0
    }

  type Trampoline[A] = Free[Function0, A]

  type FreeC[S[_], A] = Free[Coyoneda[S, ?], A]
}

import Free._

/**
 * A free operational monad for some functor `S`. Binding is done
 * using the heap instead of the stack, allowing tail-call
 * elimination.
 */
sealed abstract class Free[S[_], A] {

  final def map[B](f: A => B): Free[S, B] =
    flatMap(a => Pure(f(a)))

  /**
   * Binds the given continuation to the result of this computation.
   * All left-associated binds are reassociated to the right.
   */
  final def flatMap[B](f: A => Free[S, B]): Free[S, B] = this match {
    case a: Gosub[S, A] => gosub(a.a)(x => gosub(() => a.f(x))(f))
    case a => gosub(() => a)(f)
  }

  /**
   * Catamorphism. Run the first given function if Pure, otherwise,
   * the second given function.
   */
  final def fold[B](r: A => B, s: S[Free[S, A]] => B)(implicit S: Functor[S]): B =
    resume.fold(s, r)

  /**
   * Evaluates a single layer of the free monad.
   */
  final def resume(implicit S: Functor[S]): (Either[S[Free[S, A]], A]) = this match {
    case Pure(a) =>
      Right(a)
    case Suspend(t) =>
      Left(t)
    case x: Gosub[S, A] =>
      x.a() match {
        case Pure(a) =>
          x.f(a).resume
        case Suspend(t) =>
          Left(S.map(t)(_ flatMap x.f))
        case y: Gosub[S, x.C] =>
          y.a().flatMap(z => y.f(z) flatMap x.f).resume
      }
  }

//   /** Changes the suspension functor by the given natural transformation. */
//   final def mapSuspension[T[_]](f: S ~> T)(implicit S: Functor[S], T: Functor[T]): Free[T, A] =
//     resume match {
//       case -\/(s) => Suspend(f(S.map(s)(((_: Free[S, A]) mapSuspension f))))
//       case \/-(r) => Pure(r)
//     }
// 
//   /** Modifies the first suspension with the given natural transformation. */
//   final def mapFirstSuspension(f: S ~> S)(implicit S: Functor[S]): Free[S, A] = resume match {
//     case -\/(s) => Suspend(f(s))
//     case \/-(r) => Pure(r)
//   }
// 
//   /**
//    * Substitutes a free monad over the given functor into the suspension functor of this program.
//    * `Free` is a monad in an endofunctor category and this is its monadic bind.
//    */
//   final def flatMapSuspension[T[_]](f: S ~> Free[T, ?])(
//     implicit S: Functor[S]): Free[T, A] = resume match {
//     case -\/(s) => f(s).flatMap(_.flatMapSuspension(f))
//     case \/-(r) => Pure(r)
//   }
// 
//   /** Applies a function `f` to a value in this monad and a corresponding value in the dual comonad, annihilating both. */
//   final def zapWith[G[_], B, C](bs: Cofree[G, B])(f: (A, B) => C)(implicit S: Functor[S], G: Functor[G], d: Zap[S, G]): C =
//     Zap.monadComonadZap.zapWith(this, bs)(f)
// 
//   /** Applies a function in a comonad to the corresponding value in this monad, annihilating both. */
//   final def zap[G[_], B](fs: Cofree[G, A => B])(implicit S: Functor[S], G: Functor[G], d: Zap[S, G]): B =
//     zapWith(fs)((a, f) => f(a))
// 
//   /** Runs a single step, using a function that extracts the resumption from its suspension functor. */
//   final def bounce(f: S[Free[S, A]] => Free[S, A])(implicit S: Functor[S]): Free[S, A] = resume match {
//     case -\/(s) => f(s)
//     case \/-(r) => Pure(r)
//   }

  /**
   * Runs to completion, using a function that extracts the resumption
   * from its suspension functor.
   */
  final def go(f: S[Free[S, A]] => Free[S, A])(implicit S: Functor[S]): A = {
    @tailrec def loop(t: Free[S, A]): A =
      t.resume match {
        case Left(s) => loop(f(s))
        case Right(r) => r
      }
    loop(this)
  }

  def run(implicit S: Comonad[S]): A = go(S.extract)

  /**
   * Runs to completion, using a function that maps the resumption
   * from `S` to a monad `M`.
   */
  final def runM[M[_]](f: S[Free[S, A]] => M[Free[S, A]])(implicit S: Functor[S], M: Monad[M]): M[A] = {
    def runM2(t: Free[S, A]): M[A] = t.resume match {
      case Left(s) => Monad[M].flatMap(f(s))(runM2)
      case Right(r) => Monad[M].pure(r)
    }
    runM2(this)
  }

  /**
   * Catamorphism for `Free`.
   * 
   * Runs to completion, mapping the suspension with the given transformation at each step and
   * accumulating into the monad `M`.
   */
  final def foldMap[M[_]](f: S ~> M)(implicit S: Functor[S], M: Monad[M]): M[A] =
    this.resume match {
      case Left(s) => Monad[M].flatMap(f(s))(_.foldMap(f))
      case Right(r) => Monad[M].pure(r)
    }

//   import Id._
// 
//   /**
//    * Folds this free recursion to the right using the given natural transformations.
//    */
//   final def foldRight[G[_]](z: Id ~> G)(f: λ[α => S[G[α]]] ~> G)(implicit S: Functor[S]): G[A] =
//     this.resume match {
//       case -\/(s) => f(S.map(s)(_.foldRight(z)(f)))
//       case \/-(r) => z(r)
//     }
// 
//   /** Runs to completion, allowing the resumption function to thread an arbitrary state of type `B`. */
//   final def foldRun[B](b: B)(f: (B, S[Free[S, A]]) => (B, Free[S, A]))(implicit S: Functor[S]): (B, A) = {
//     @tailrec def foldRun2(t: Free[S, A], z: B): (B, A) = t.resume match {
//       case -\/(s) =>
//         val (b1, s1) = f(z, s)
//         foldRun2(s1, b1)
//       case \/-(r) => (z, r)
//     }
//     foldRun2(this, b)
//   }


//   /** Interleave this computation with another, combining the results with the given function. */
//   def zipWith[B, C](tb: Free[S, B])(f: (A, B) => C)(implicit S: Functor[S]): Free[S, C] = {
//     (resume, tb.resume) match {
//       case (-\/(a), -\/(b)) => Suspend(S.map(a)(x => Suspend(S.map(b)(y => x.zipWith(y)(f)))))
//       case (-\/(a), \/-(b)) => Suspend(S.map(a)(x => x.zipWith(Pure(b))(f)))
//       case (\/-(a), -\/(b)) => Suspend(S.map(b)(y => Pure(a).zipWith(y)(f)))
//       case (\/-(a), \/-(b)) => Pure(f(a, b))
//     }
//   }
// 
//   /** Runs a `Source` all the way to the end, tail-recursively, collecting the produced values. */
//   def collect[B](implicit ev: Free[S, A] =:= Source[B, A]): (Vector[B], A) = {
//     @tailrec def go(c: Source[B, A], v: Vector[B] = Vector()): (Vector[B], A) =
//       c.resume match {
//         case -\/((b, cont)) => go(cont, v :+ b)
//         case \/-(r)         => (v, r)
//       }
//     go(ev(this))
//   }
// 
//   /** Drive this `Source` with the given Sink. */
//   def drive[E, B](sink: Sink[Option[E], B])(implicit ev: Free[S, A] =:= Source[E, A]): (A, B) = {
//     @tailrec def go(src: Source[E, A], snk: Sink[Option[E], B]): (A, B) =
//       (src.resume, snk.resume) match {
//         case (-\/((e, c)), -\/(f)) => go(c, f(Some(e)))
//         case (-\/((e, c)), \/-(y)) => go(c, Sink.sinkMonad[Option[E]].pure(y))
//         case (\/-(x), -\/(f))      => go(Source.sourceMonad[E].pure(x), f(None))
//         case (\/-(x), \/-(y))      => (x, y)
//       }
//     go(ev(this), sink)
//   }
// 
//   /** Feed the given stream to this `Source`. */
//   def feed[E](ss: Stream[E])(implicit ev: Free[S, A] =:= Sink[E, A]): A = {
//     @tailrec def go(snk: Sink[E, A], rest: Stream[E]): A = (rest, snk.resume) match {
//       case (x #:: xs, -\/(f)) => go(f(x), xs)
//       case (Stream(), -\/(f)) => go(f(sys.error("No more values.")), Stream())
//       case (_, \/-(r))        => r
//     }
//     go(ev(this), ss)
//   }
// 
//   /** Feed the given source to this `Sink`. */
//   def drain[E, B](source: Source[E, B])(implicit ev: Free[S, A] =:= Sink[E, A]): (A, B) = {
//     @tailrec def go(src: Source[E, B], snk: Sink[E, A]): (A, B) = (src.resume, snk.resume) match {
//       case (-\/((e, c)), -\/(f)) => go(c, f(e))
//       case (-\/((e, c)), \/-(y)) => go(c, Sink.sinkMonad[E].pure(y))
//       case (\/-(x), -\/(f))      => sys.error("Not enough values in source.")
//       case (\/-(x), \/-(y))      => (y, x)
//     }
//     go(source, ev(this))
//   }
// }
}

object Trampoline {
  def done[A](a: A): Trampoline[A] =
    Free.Pure[Function0,A](a)

  def suspend[A](a: => Trampoline[A]): Trampoline[A] =
    Free.Suspend[Function0, A](() => a)

  def delay[A](a: => A): Trampoline[A] =
    suspend(done(a))

  // implicit val instance: Monad[Trampoline] with Comonad[Trampoline] =
  //   new Monad[Trampoline] with Comonad[Trampoline] {
  //     override def point[A](a: => A) = ??? //return_[Function0, A](a)
  //     def bind[A, B](ta: Trampoline[A])(f: A => Trampoline[B]) = ta.flatMap(f)
  //     def copoint[A](fa: Trampoline[A]) = fa.run
  //     def cobind[A, B](fa: Trampoline[A])(f: Trampoline[A] => B) = ??? //return_(f(fa))
  //     override def cojoin[A](fa: Trampoline[A]) = Pure(fa)
  //   }
}

// sealed trait TrampolineInstances {
//   implicit val trampolineInstance: Monad[Trampoline] with Comonad[Trampoline] =
//     new Monad[Trampoline] with Comonad[Trampoline] {
//       override def point[A](a: => A) = return_[Function0, A](a)
//       def bind[A, B](ta: Trampoline[A])(f: A => Trampoline[B]) = ta flatMap f
//       def copoint[A](fa: Trampoline[A]) = fa.run
//       def cobind[A, B](fa: Trampoline[A])(f: Trampoline[A] => B) = return_(f(fa))
//       override def cojoin[A](fa: Trampoline[A]) = Pure(fa)
//     }
// }
// 
// object Sink extends SinkInstances
// 
// sealed trait SinkInstances {
//   implicit def sinkMonad[S]: Monad[Sink[S, ?]] =
//     new Monad[Sink[S, ?]] {
//       def point[A](a: => A) =
//         Suspend[(=> S) => ?, A](s =>
//          Pure[(=> S) => ?, A](a))
//       def bind[A, B](s: Sink[S, A])(f: A => Sink[S, B]) = s flatMap f
//     }
// }
// 
// object Source extends SourceInstances
// 
// sealed trait SourceInstances {
//   implicit def sourceMonad[S]: Monad[Source[S, ?]] =
//     new Monad[Source[S, ?]] {
//       override def point[A](a: => A) = Pure[(S, ?), A](a)
//       def bind[A, B](s: Source[S, A])(f: A => Source[S, B]) = s flatMap f
//     }
// }
// 
// sealed abstract class FreeInstances3 {
//   implicit def freeFoldable[F[_]: Foldable: Functor]: Foldable[Free[F, ?]] =
//     new FreeFoldable[F] {
//       def F = implicitly
//       def F0 = implicitly
//     }
// }
// 
// sealed abstract class FreeInstances2 extends FreeInstances3 {
//   implicit def freeFoldable1[F[_]: Foldable1: Functor]: Foldable1[Free[F, ?]] =
//     new FreeFoldable1[F] {
//       def F = implicitly
//       def F0 = implicitly
//     }
// }
// 
// sealed abstract class FreeInstances1 extends FreeInstances2 {
//   implicit def freeTraverse[F[_]: Traverse]: Traverse[Free[F, ?]] =
//     new FreeTraverse[F] {
//       def F = implicitly
//     }
// }
// 
// sealed abstract class FreeInstances0 extends FreeInstances1 {
//   implicit def freeTraverse1[F[_]: Traverse1]: Traverse1[Free[F, ?]] =
//     new FreeTraverse1[F] {
//       def F = implicitly
//     }
// 
//   implicit def freeSemigroup[S[_]:Functor, A:Semigroup]: Semigroup[Free[S, A]] =
//     Semigroup.liftSemigroup[Free[S, ?], A]
// }
// 
// // Trampoline, Sink, and Source are type aliases. We need to add their type class instances
// // to Free to be part of the implicit scope.
// sealed abstract class FreeInstances extends FreeInstances0 with TrampolineInstances with SinkInstances with SourceInstances {
//   implicit def freeMonad[S[_]:Functor]: Monad[Free[S, ?]] =
//     new Monad[Free[S, ?]] {
//       def point[A](a: => A) = Pure(a)
//       override def map[A, B](fa: Free[S, A])(f: A => B) = fa map f
//       def bind[A, B](a: Free[S, A])(f: A => Free[S, B]) = a flatMap f
//     }
// 
//   implicit def freeMonoid[S[_]:Functor, A:Monoid]: Monoid[Free[S, A]] =
//     Monoid.liftMonoid[Free[S, ?], A]
// }
// 
// trait FreeFunctions {
//   /** Collapse a trampoline to a single step. */
//   def reset[A](r: Trampoline[A]): Trampoline[A] = { val a = r.run; return_(a) }
// 
//   /** Suspend the given computation in a single step. */
//   def return_[S[_], A](value: => A)(implicit S: Applicative[S]): Free[S, A] =
//     Suspend[S, A](S.point(Pure[S, A](value)))
// 
//   /** Pure the given value in the free monad. */
//   def point[S[_], A](value: => A): Free[S, A] = Pure[S, A](value)
// 
//   /** Alias for `point` */
//   def pure[S[_], A](value: => A): Free[S, A] = point(value)
// 
//   def suspend[S[_], A](value: => Free[S, A])(implicit S: Applicative[S]): Free[S, A] =
//     Suspend[S, A](S.point(value))
// 
//   /** Suspends a value within a functor in a single step. Monadic unit for a higher-order monad. */
//   def liftF[S[_], A](value: => S[A])(implicit S: Functor[S]): Free[S, A] =
//     Suspend(S.map(value)(Pure[S, A]))
// 
//   /** A version of `liftF` that infers the nested type constructor. */
//   def liftFU[MA](value: => MA)(implicit MA: Unapply[Functor, MA]): Free[MA.M, MA.A] =
//     liftF(MA(value))(MA.TC)
// 
//   /** A free monad over a free functor of `S`. */
//   def liftFC[S[_], A](s: S[A]): FreeC[S, A] =
//     liftFU(Coyoneda lift s)
// 
//   /** Monadic join for the higher-order monad `Free` */
//   def joinF[S[_], A](value: Free[Free[S, ?], A])(implicit S: Functor[S]): Free[S, A] =
//     value.flatMapSuspension(NaturalTransformation.refl[Free[S, ?]])
// 
//   /** Interpret a free monad over a free functor of `S` via natural transformation to monad `M`. */
//   def runFC[S[_], M[_], A](sa: FreeC[S, A])(interp: S ~> M)(implicit M: Monad[M]): M[A] =
//     sa.foldMap[M](new (Coyoneda[S, ?] ~> M) {
//       def apply[A](cy: Coyoneda[S, A]): M[A] =
//         M.map(interp(cy.fi))(cy.k)
//       })
// 
//   /** A trampoline step that doesn't do anything. */
//   def pause: Trampoline[Unit] =
//     return_(())
// 
//   /** A source that produces the given value. */
//   def produce[A](a: A): Source[A, Unit] =
//     Suspend[(A, ?), Unit](a -> Pure[(A, ?), Unit](()))
// 
//   /** A sink that waits for a single value and returns it. */
//   def await[A]: Sink[A, A] =
//     Suspend[(=> A) => ?, A](a => Pure[(=> A) => ?, A](a))
// }
// 
// private sealed trait FreeFoldable[F[_]] extends Foldable[Free[F, ?]] {
//   def F: Foldable[F]
//   implicit def F0: Functor[F]
// 
//   override final def foldMap[A, B: Monoid](fa: Free[F, A])(f: A => B): B =
//     fa.resume match {
//       case -\/(s) => F.foldMap(s)(foldMap(_)(f))
//       case \/-(r) => f(r)
//     }
// 
//   override final def foldLeft[A, B](fa: Free[F, A], z: B)(f: (B, A) => B): B =
//     fa.resume match {
//       case -\/(s) => F.foldLeft(s, z)((b, a) => foldLeft(a, b)(f))
//       case \/-(r) => f(z, r)
//     }
// 
//   override final def foldRight[A, B](fa: Free[F, A], z: => B)(f: (A, => B) => B): B =
//     fa.resume match {
//       case -\/(s) => F.foldRight(s, z)(foldRight(_, _)(f))
//       case \/-(r) => f(r, z)
//     }
// }
// 
// private sealed trait FreeFoldable1[F[_]] extends Foldable1[Free[F, ?]] {
//   def F: Foldable1[F]
//   implicit def F0: Functor[F]
// 
//   override final def foldMap1[A, B: Semigroup](fa: Free[F, A])(f: A => B): B =
//     fa.resume match {
//       case -\/(s) => F.foldMap1(s)(foldMap1(_)(f))
//       case \/-(r) => f(r)
//     }
// 
//   override final def foldMapRight1[A, B](fa: Free[F, A])(z: A => B)(f: (A, => B) => B): B =
//     fa.resume match {
//       case -\/(s) => F.foldMapRight1(s)(foldMapRight1(_)(z)(f))(foldRight(_, _)(f))
//       case \/-(r) => z(r)
//     }
// 
//   override final def foldMapLeft1[A, B](fa: Free[F, A])(z: A => B)(f: (B, A) => B): B =
//     fa.resume match {
//       case -\/(s) => F.foldMapLeft1(s)(foldMapLeft1(_)(z)(f))((b, a) => foldLeft(a, b)(f))
//       case \/-(r) => z(r)
//     }
// }
// 
// private sealed trait FreeTraverse[F[_]] extends Traverse[Free[F, ?]] with FreeFoldable[F]{
//   implicit def F: Traverse[F]
//   override final def F0 = F
// 
//   override final def map[A, B](fa: Free[F, A])(f: A => B) = fa map f
// 
//   override final def traverseImpl[G[_], A, B](fa: Free[F, A])(f: A => G[B])(implicit G: Applicative[G]): G[Free[F, B]] =
//     fa.resume match {
//       case -\/(s) => G.map(F.traverseImpl(s)(traverseImpl[G, A, B](_)(f)))(Suspend(_))
//       case \/-(r) => G.map(f(r))(Pure(_))
//     }
// }
// 
// private sealed abstract class FreeTraverse1[F[_]] extends Traverse1[Free[F, ?]] with FreeTraverse[F] with FreeFoldable1[F]{
//   implicit def F: Traverse1[F]
// 
//   override final def traverse1Impl[G[_], A, B](fa: Free[F, A])(f: A => G[B])(implicit G: Apply[G]): G[Free[F, B]] =
//     fa.resume match {
//       case -\/(s) => G.map(F.traverse1Impl(s)(traverse1Impl[G, A, B](_)(f)))(Suspend(_))
//       case \/-(r) => G.map(f(r))(Pure(_))
//     }
//}
