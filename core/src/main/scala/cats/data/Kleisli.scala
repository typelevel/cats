package cats
package data

import cats.arrow.{Arrow, Choice, Split, FunctionK}
import cats.functor.{Contravariant, Strong}

/**
 * Represents a function `A => F[B]`.
 */
final case class Kleisli[F[_], A, B](run: A => F[B]) { self =>

  def ap[C](f: Kleisli[F, A, B => C])(implicit F: Apply[F]): Kleisli[F, A, C] =
    Kleisli(a => F.ap(f.run(a))(run(a)))

  def dimap[C, D](f: C => A)(g: B => D)(implicit F: Functor[F]): Kleisli[F, C, D] =
    Kleisli(c => F.map(run(f(c)))(g))

  def map[C](f: B => C)(implicit F: Functor[F]): Kleisli[F, A, C] =
    Kleisli(a => F.map(run(a))(f))

  def mapF[N[_], C](f: F[B] => N[C]): Kleisli[N, A, C] =
    Kleisli(run andThen f)

  def flatMap[C](f: B => Kleisli[F, A, C])(implicit F: FlatMap[F]): Kleisli[F, A, C] =
    Kleisli((r: A) => F.flatMap[B, C](run(r))((b: B) => f(b).run(r)))

  def flatMapF[C](f: B => F[C])(implicit F: FlatMap[F]): Kleisli[F, A, C] =
    Kleisli(a => F.flatMap(run(a))(f))

  def andThen[C](f: B => F[C])(implicit F: FlatMap[F]): Kleisli[F, A, C] =
    Kleisli((a: A) => F.flatMap(run(a))(f))

  def andThen[C](k: Kleisli[F, B, C])(implicit F: FlatMap[F]): Kleisli[F, A, C] =
    this andThen k.run

  def compose[Z](f: Z => F[A])(implicit F: FlatMap[F]): Kleisli[F, Z, B] =
    Kleisli((z: Z) => F.flatMap(f(z))(run))

  def compose[Z](k: Kleisli[F, Z, A])(implicit F: FlatMap[F]): Kleisli[F, Z, B] =
    this compose k.run

  def traverse[G[_]](f: G[A])(implicit F: Applicative[F], G: Traverse[G]): F[G[B]] =
    G.traverse(f)(run)

  def lift[G[_]](implicit G: Applicative[G]): Kleisli[λ[α => G[F[α]]], A, B] =
    Kleisli[λ[α => G[F[α]]], A, B](a => Applicative[G].pure(run(a)))

  def local[AA](f: AA => A): Kleisli[F, AA, B] =
    Kleisli(f.andThen(run))

  def transform[G[_]](f: FunctionK[F, G]): Kleisli[G, A, B] =
    Kleisli(a => f(run(a)))

  def lower(implicit F: Applicative[F]): Kleisli[F, A, F[B]] =
    Kleisli(a => F.pure(run(a)))

  def first[C](implicit F: Functor[F]): Kleisli[F, (A, C), (B, C)] =
    Kleisli{ case (a, c) => F.fproduct(run(a))(_ => c)}

  def second[C](implicit F: Functor[F]): Kleisli[F, (C, A), (C, B)] =
    Kleisli{ case (c, a) => F.map(run(a))(c -> _)}

  def apply(a: A): F[B] = run(a)
}

object Kleisli extends KleisliInstances with KleisliFunctions

private[data] sealed trait KleisliFunctions {

  def lift[F[_], A, B](x: F[B]): Kleisli[F, A, B] =
    Kleisli(_ => x)

  def pure[F[_], A, B](x: B)(implicit F: Applicative[F]): Kleisli[F, A, B] =
    Kleisli(_ => F.pure(x))

  def ask[F[_], A](implicit F: Applicative[F]): Kleisli[F, A, A] =
    Kleisli(F.pure)

  def local[M[_], A, R](f: R => R)(fa: Kleisli[M, R, A]): Kleisli[M, R, A] =
    Kleisli(f andThen fa.run)
}

private[data] sealed abstract class KleisliInstances extends KleisliInstances0 {

  implicit def catsDataMonoidForKleisli[F[_], A, B](implicit M: Monoid[F[B]]): Monoid[Kleisli[F, A, B]] =
    new KleisliMonoid[F, A, B] { def FB: Monoid[F[B]] = M }

  implicit def catsDataMonoidKForKleisli[F[_]](implicit M: Monad[F]): MonoidK[λ[α => Kleisli[F, α, α]]] =
    new KleisliMonoidK[F] { def F: Monad[F] = M }

  implicit val catsDataMonoidKForKleisliId: MonoidK[λ[α => Kleisli[Id, α, α]]] =
    catsDataMonoidKForKleisli[Id]

  implicit def catsDataArrowForKleisli[F[_]](implicit ev: Monad[F]): Arrow[Kleisli[F, ?, ?]] =
    new KleisliArrow[F] { def F: Monad[F] = ev }

  implicit val catsDataArrowForKleisliId: Arrow[Kleisli[Id, ?, ?]] =
    catsDataArrowForKleisli[Id]

  implicit def catsDataChoiceForKleisli[F[_]](implicit ev: Monad[F]): Choice[Kleisli[F, ?, ?]] =
    new Choice[Kleisli[F, ?, ?]] {
      def id[A]: Kleisli[F, A, A] = Kleisli(ev.pure(_))

      def choice[A, B, C](f: Kleisli[F, A, C], g: Kleisli[F, B, C]): Kleisli[F, Xor[A, B], C] =
        Kleisli(_.fold(f.run, g.run))

      def compose[A, B, C](f: Kleisli[F, B, C], g: Kleisli[F, A, B]): Kleisli[F, A, C] =
        f.compose(g)
    }

  implicit val catsDataChoiceForKleisliId: Choice[Kleisli[Id, ?, ?]] =
    catsDataChoiceForKleisli[Id]

  implicit def catsDataMonadReaderForKleisliId[A]: MonadReader[Kleisli[Id, A, ?], A] =
    catsDataMonadReaderForKleisli[Id, A]

  implicit def catsDataContravariantForKleisli[F[_], C]: Contravariant[Kleisli[F, ?, C]] =
    new Contravariant[Kleisli[F, ?, C]] {
      override def contramap[A, B](fa: Kleisli[F, A, C])(f: (B) => A): Kleisli[F, B, C] =
        fa.local(f)
    }

  implicit def catsDataTransLiftForKleisli[A]: TransLift.AuxId[Kleisli[?[_], A, ?]] =
    new TransLift[Kleisli[?[_], A, ?]] {
      type TC[M[_]] = Trivial

      def liftT[M[_], B](ma: M[B])(implicit ev: Trivial): Kleisli[M, A, B] = Kleisli.lift(ma)
    }

  implicit def catsDataApplicativeErrorForKleisli[F[_], A, E](implicit AE: ApplicativeError[F, E]): ApplicativeError[Kleisli[F, A, ?], E]
    = new KleisliApplicativeError[F, A, E] { implicit def AF: ApplicativeError[F, E]  = AE }
}

private[data] sealed abstract class KleisliInstances0 extends KleisliInstances1 {
  implicit def catsDataSplitForKleisli[F[_]](implicit ev: FlatMap[F]): Split[Kleisli[F, ?, ?]] =
    new KleisliSplit[F] { def F: FlatMap[F] = ev }

  implicit def catsDataStrongForKleisli[F[_]](implicit ev: Functor[F]): Strong[Kleisli[F, ?, ?]] =
    new KleisliStrong[F] { def F: Functor[F] = ev }

  implicit def catsDataFlatMapForKleisli[F[_]: FlatMap, A]: FlatMap[Kleisli[F, A, ?]] = new FlatMap[Kleisli[F, A, ?]] {
    def flatMap[B, C](fa: Kleisli[F, A, B])(f: B => Kleisli[F, A, C]): Kleisli[F, A, C] =
      fa.flatMap(f)

    def map[B, C](fa: Kleisli[F, A, B])(f: B => C): Kleisli[F, A, C] =
      fa.map(f)

    def tailRecM[B, C](b: B)(f: B => Kleisli[F, A, Either[B, C]]): Kleisli[F, A, C] =
      Kleisli[F, A, C]({ a => FlatMap[F].tailRecM(b) { f(_).run(a) } })
  }

  implicit def catsDataRecursiveTailRecMForKleisli[F[_]: RecursiveTailRecM, A]: RecursiveTailRecM[Kleisli[F, A, ?]] =
    RecursiveTailRecM.create[Kleisli[F, A, ?]]

  implicit def catsDataSemigroupForKleisli[F[_], A, B](implicit M: Semigroup[F[B]]): Semigroup[Kleisli[F, A, B]] =
    new KleisliSemigroup[F, A, B] { def FB: Semigroup[F[B]] = M }

  implicit def catsDataSemigroupKForKleisli[F[_]](implicit ev: FlatMap[F]): SemigroupK[λ[α => Kleisli[F, α, α]]] =
    new KleisliSemigroupK[F] { def F: FlatMap[F] = ev }

}

private[data] sealed abstract class KleisliInstances1 extends KleisliInstances2 {
  implicit def catsDataApplicativeForKleisli[F[_], A](implicit A : Applicative[F]): Applicative[Kleisli[F, A, ?]] = new KleisliApplicative[F, A] {
    implicit def F: Applicative[F] = A
  }
}

private[data] sealed abstract class KleisliInstances2 extends KleisliInstances3 {
  implicit def catsDataApplyForKleisli[F[_]: Apply, A]: Apply[Kleisli[F, A, ?]] = new Apply[Kleisli[F, A, ?]] {
    def ap[B, C](f: Kleisli[F, A, B => C])(fa: Kleisli[F, A, B]): Kleisli[F, A, C] =
      fa.ap(f)

    override def product[B, C](fb: Kleisli[F, A, B], fc: Kleisli[F, A, C]): Kleisli[F, A, (B, C)] =
      Kleisli(a => Apply[F].product(fb.run(a), fc.run(a)))

    def map[B, C](fa: Kleisli[F, A, B])(f: B => C): Kleisli[F, A, C] =
      fa.map(f)
  }
}

private[data] sealed abstract class KleisliInstances3 extends KleisliInstances4 {
  implicit def catsDataFunctorForKleisli[F[_]: Functor, A]: Functor[Kleisli[F, A, ?]] = new Functor[Kleisli[F, A, ?]] {
    def map[B, C](fa: Kleisli[F, A, B])(f: B => C): Kleisli[F, A, C] =
      fa.map(f)
  }
}

private[data] sealed abstract class KleisliInstances4 {

  implicit def catsDataMonadReaderForKleisli[F[_]: Monad, A]: MonadReader[Kleisli[F, A, ?], A] =
    new MonadReader[Kleisli[F, A, ?], A] {
      def pure[B](x: B): Kleisli[F, A, B] =
        Kleisli.pure[F, A, B](x)

      def flatMap[B, C](fa: Kleisli[F, A, B])(f: B => Kleisli[F, A, C]): Kleisli[F, A, C] =
        fa.flatMap(f)

      val ask: Kleisli[F, A, A] = Kleisli(Monad[F].pure)

      def local[B](f: A => A)(fa: Kleisli[F, A, B]): Kleisli[F, A, B] =
        Kleisli(f.andThen(fa.run))

      def tailRecM[B, C](b: B)(f: B => Kleisli[F, A, Either[B, C]]): Kleisli[F, A, C] =
        Kleisli[F, A, C]({ a => FlatMap[F].tailRecM(b) { f(_).run(a) } })
    }
}

private trait KleisliArrow[F[_]] extends Arrow[Kleisli[F, ?, ?]] with KleisliSplit[F] with KleisliStrong[F] {
  implicit def F: Monad[F]

  def lift[A, B](f: A => B): Kleisli[F, A, B] =
    Kleisli(a => F.pure(f(a)))

  def id[A]: Kleisli[F, A, A] =
    Kleisli(a => F.pure(a))

  override def second[A, B, C](fa: Kleisli[F, A, B]): Kleisli[F, (C, A), (C, B)] =
    super[KleisliStrong].second(fa)

  override def split[A, B, C, D](f: Kleisli[F, A, B], g: Kleisli[F, C, D]): Kleisli[F, (A, C), (B, D)] =
    super[KleisliSplit].split(f, g)
}

private trait KleisliSplit[F[_]] extends Split[Kleisli[F, ?, ?]] {
  implicit def F: FlatMap[F]

  def split[A, B, C, D](f: Kleisli[F, A, B], g: Kleisli[F, C, D]): Kleisli[F, (A, C), (B, D)] =
    Kleisli{ case (a, c) => F.flatMap(f.run(a))(b => F.map(g.run(c))(d => (b, d))) }

  def compose[A, B, C](f: Kleisli[F, B, C], g: Kleisli[F, A, B]): Kleisli[F, A, C] =
    f.compose(g)
}

private trait KleisliStrong[F[_]] extends Strong[Kleisli[F, ?, ?]] {
  implicit def F: Functor[F]

  override def lmap[A, B, C](fab: Kleisli[F, A, B])(f: C => A): Kleisli[F, C, B] =
    fab.local(f)

  override def rmap[A, B, C](fab: Kleisli[F, A, B])(f: B => C): Kleisli[F, A, C] =
    fab.map(f)

  override def dimap[A, B, C, D](fab: Kleisli[F, A, B])(f: C => A)(g: B => D): Kleisli[F, C, D] =
    fab.dimap(f)(g)

  def first[A, B, C](fa: Kleisli[F, A, B]): Kleisli[F, (A, C), (B, C)] =
    fa.first[C]

  def second[A, B, C](fa: Kleisli[F, A, B]): Kleisli[F, (C, A), (C, B)] =
    fa.second[C]
}

private trait KleisliSemigroup[F[_], A, B] extends Semigroup[Kleisli[F, A, B]] {
  implicit def FB: Semigroup[F[B]]

  override def combine(a: Kleisli[F, A, B], b: Kleisli[F, A, B]): Kleisli[F, A, B] =
    Kleisli[F, A, B](x => FB.combine(a.run(x), b.run(x)))
}

private trait KleisliMonoid[F[_], A, B] extends Monoid[Kleisli[F, A, B]] with KleisliSemigroup[F, A, B] {
  implicit def FB: Monoid[F[B]]

  override def empty: Kleisli[F, A, B] = Kleisli[F, A, B](a => FB.empty)
}

private trait KleisliSemigroupK[F[_]] extends SemigroupK[λ[α => Kleisli[F, α, α]]] {
  implicit def F: FlatMap[F]

  override def combineK[A](a: Kleisli[F, A, A], b: Kleisli[F, A, A]): Kleisli[F, A, A] = a compose b
}

private trait KleisliMonoidK[F[_]] extends MonoidK[λ[α => Kleisli[F, α, α]]] with KleisliSemigroupK[F] {
  implicit def F: Monad[F]

  override def empty[A]: Kleisli[F, A, A] = Kleisli(F.pure[A])
}


private trait KleisliApplicativeError[F[_], A, E] extends KleisliApplicative[F, A] with ApplicativeError[Kleisli[F, A, ?], E] {
  type K[T] = Kleisli[F, A, T]

  implicit def AF: ApplicativeError[F, E]

  implicit def F: Applicative[F] = AF

  def raiseError[B](e: E): K[B] = Kleisli(_ => AF.raiseError(e))

  def handleErrorWith[B](kb: K[B])(f: E => K[B]): K[B] = Kleisli { a: A =>
    AF.handleErrorWith(kb.run(a))((e: E) => f(e).run(a))
  }

}


private trait KleisliApplicative[F[_], A] extends Applicative[Kleisli[F, A, ?]] {
  implicit def F: Applicative[F]

  def pure[B](x: B): Kleisli[F, A, B] =
    Kleisli.pure[F, A, B](x)

  def ap[B, C](f: Kleisli[F, A, B => C])(fa: Kleisli[F, A, B]): Kleisli[F, A, C] =
    fa.ap(f)

  override def map[B, C](fb: Kleisli[F, A, B])(f: B => C): Kleisli[F, A, C] =
    fb.map(f)

  override def product[B, C](fb: Kleisli[F, A, B], fc: Kleisli[F, A, C]): Kleisli[F, A, (B, C)] =
    Kleisli(a => Applicative[F].product(fb.run(a), fc.run(a)))
}
