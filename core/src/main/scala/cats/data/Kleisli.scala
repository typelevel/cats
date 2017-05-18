package cats
package data

import cats.arrow.{Arrow, Category, Choice, Compose, Split, FunctionK}
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

  implicit def catsDataMonoidForKleisli[F[_], A, B](implicit FB0: Monoid[F[B]]): Monoid[Kleisli[F, A, B]] =
    new KleisliMonoid[F, A, B] { def FB: Monoid[F[B]] = FB0 }

  implicit def catsDataMonoidKForKleisli[F[_]](implicit M: Monad[F]): MonoidK[λ[α => Kleisli[F, α, α]]] =
    Category[Kleisli[F, ?, ?]].algebraK

  implicit val catsDataMonoidKForKleisliId: MonoidK[λ[α => Kleisli[Id, α, α]]] =
    catsDataMonoidKForKleisli[Id]

  implicit def catsDataArrowForKleisli[F[_]](implicit M: Monad[F]): Arrow[Kleisli[F, ?, ?]] =
    new KleisliArrow[F] { def F: Monad[F] = M }

  implicit val catsDataArrowForKleisliId: Arrow[Kleisli[Id, ?, ?]] =
    catsDataArrowForKleisli[Id]

  implicit def catsDataMonadReaderForKleisliId[A]: MonadReader[Kleisli[Id, A, ?], A] =
    catsDataMonadReaderForKleisli[Id, A]

  implicit def catsDataContravariantForKleisli[F[_], C]: Contravariant[Kleisli[F, ?, C]] =
    new Contravariant[Kleisli[F, ?, C]] {
      override def contramap[A, B](fa: Kleisli[F, A, C])(f: B => A): Kleisli[F, B, C] =
        fa.local(f)
    }

  implicit def catsDataMonadTransForKleisli[A]: MonadTrans[Kleisli[?[_], A, ?]] =
    new MonadTrans[Kleisli[?[_], A, ?]] {
      def liftT[M[_]: Monad, B](ma: M[B]): Kleisli[M, A, B] = Kleisli.lift(ma)
    }

  implicit def catsDataApplicativeErrorForKleisli[F[_], A, E](implicit AE: ApplicativeError[F, E]): ApplicativeError[Kleisli[F, A, ?], E] =
    new KleisliApplicativeError[F, A, E] { def F: ApplicativeError[F, E] = AE }
}

private[data] sealed abstract class KleisliInstances0 extends KleisliInstances1 {
  implicit def catsDataMonadErrorForKleisli[F[_], A, E](implicit ME: MonadError[F, E]): MonadError[Kleisli[F, A, ?], E] =
    new KleisliMonadError[F, A, E] { def F: MonadError[F, E] = ME }
}

private[data] sealed abstract class KleisliInstances1 extends KleisliInstances2 {
  implicit def catsDataMonadReaderForKleisli[F[_], A](implicit M: Monad[F]): MonadReader[Kleisli[F, A, ?], A] =
    new KleisliMonadReader[F, A] { def F: Monad[F] = M }
}

private[data] sealed abstract class KleisliInstances2 extends KleisliInstances3 {
  implicit def catsDataChoiceForKleisli[F[_]](implicit M: Monad[F]): Choice[Kleisli[F, ?, ?]] =
    new KleisliChoice[F] { def F: Monad[F] = M }

  implicit val catsDataChoiceForKleisliId: Choice[Kleisli[Id, ?, ?]] =
    catsDataChoiceForKleisli[Id]

  implicit def catsDataSplitForKleisli[F[_]](implicit FM: FlatMap[F]): Split[Kleisli[F, ?, ?]] =
    new KleisliSplit[F] { def F: FlatMap[F] = FM }

  implicit def catsDataStrongForKleisli[F[_]](implicit F0: Functor[F]): Strong[Kleisli[F, ?, ?]] =
    new KleisliStrong[F] { def F: Functor[F] = F0 }

  implicit def catsDataFlatMapForKleisli[F[_], A](implicit FM: FlatMap[F]): FlatMap[Kleisli[F, A, ?]] =
    new KleisliFlatMap[F, A] { def F: FlatMap[F] = FM }

  implicit def catsDataSemigroupForKleisli[F[_], A, B](implicit FB0: Semigroup[F[B]]): Semigroup[Kleisli[F, A, B]] =
    new KleisliSemigroup[F, A, B] { def FB: Semigroup[F[B]] = FB0 }

  implicit def catsDataSemigroupKForKleisli[F[_]](implicit FM: FlatMap[F]): SemigroupK[λ[α => Kleisli[F, α, α]]] =
    Compose[Kleisli[F, ?, ?]].algebraK
}

private[data] sealed abstract class KleisliInstances3 extends KleisliInstances4 {
  implicit def catsDataApplicativeForKleisli[F[_], A](implicit A: Applicative[F]): Applicative[Kleisli[F, A, ?]] =
    new KleisliApplicative[F, A] { def F: Applicative[F] = A }
}

private[data] sealed abstract class KleisliInstances4 extends KleisliInstances5 {
  implicit def catsDataApplyForKleisli[F[_], A](implicit A: Apply[F]): Apply[Kleisli[F, A, ?]] =
    new KleisliApply[F, A] { def F: Apply[F] = A }
}

private[data] sealed abstract class KleisliInstances5 {
  implicit def catsDataFunctorForKleisli[F[_], A](implicit F0: Functor[F]): Functor[Kleisli[F, A, ?]] =
    new KleisliFunctor[F, A] { def F: Functor[F] = F0 }
}

private trait KleisliArrow[F[_]] extends Arrow[Kleisli[F, ?, ?]] with KleisliSplit[F] with KleisliStrong[F] with KleisliCategory[F] {
  implicit def F: Monad[F]

  def lift[A, B](f: A => B): Kleisli[F, A, B] =
    Kleisli(a => F.pure(f(a)))
}

private trait KleisliSplit[F[_]] extends Split[Kleisli[F, ?, ?]] with KleisliCompose[F] {
  implicit def F: FlatMap[F]

  override def split[A, B, C, D](f: Kleisli[F, A, B], g: Kleisli[F, C, D]): Kleisli[F, (A, C), (B, D)] =
    Kleisli{ case (a, c) => F.flatMap(f.run(a))(b => F.map(g.run(c))(d => (b, d))) }
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

  override def second[A, B, C](fa: Kleisli[F, A, B]): Kleisli[F, (C, A), (C, B)] =
    fa.second[C]
}

private trait KleisliChoice[F[_]] extends Choice[Kleisli[F, ?, ?]] with KleisliCategory[F] {
  def choice[A, B, C](f: Kleisli[F, A, C], g: Kleisli[F, B, C]): Kleisli[F, Either[A, B], C] =
    Kleisli(_.fold(f.run, g.run))
}

private trait KleisliCategory[F[_]] extends Category[Kleisli[F, ?, ?]] with KleisliCompose[F] {
  implicit def F: Monad[F]

  def id[A]: Kleisli[F, A, A] = Kleisli.ask[F, A]
}

private trait KleisliCompose[F[_]] extends Compose[Kleisli[F, ?, ?]] {
  implicit def F: FlatMap[F]

  def compose[A, B, C](f: Kleisli[F, B, C], g: Kleisli[F, A, B]): Kleisli[F, A, C] =
    f.compose(g)
}

private trait KleisliSemigroup[F[_], A, B] extends Semigroup[Kleisli[F, A, B]] {
  implicit def FB: Semigroup[F[B]]

  override def combine(a: Kleisli[F, A, B], b: Kleisli[F, A, B]): Kleisli[F, A, B] =
    Kleisli[F, A, B](x => FB.combine(a.run(x), b.run(x)))
}

private trait KleisliMonoid[F[_], A, B] extends Monoid[Kleisli[F, A, B]] with KleisliSemigroup[F, A, B] {
  implicit def FB: Monoid[F[B]]

  override def empty: Kleisli[F, A, B] = Kleisli[F, A, B](_ => FB.empty)
}

private trait KleisliMonadError[F[_], A, E] extends MonadError[Kleisli[F, A, ?], E] with KleisliApplicativeError[F, A, E] with KleisliMonadReader[F, A] {
  def F: MonadError[F, E]
}

private trait KleisliApplicativeError[F[_], A, E] extends ApplicativeError[Kleisli[F, A, ?], E] with KleisliApplicative[F, A] {
  type K[T] = Kleisli[F, A, T]

  implicit def F: ApplicativeError[F, E]

  def raiseError[B](e: E): K[B] = Kleisli(_ => F.raiseError(e))

  def handleErrorWith[B](kb: K[B])(f: E => K[B]): K[B] = Kleisli { a: A =>
    F.handleErrorWith(kb.run(a))((e: E) => f(e).run(a))
  }
}

private trait KleisliMonadReader[F[_], A] extends MonadReader[Kleisli[F, A, ?], A] with KleisliMonad[F, A] {
  implicit def F: Monad[F]

  val ask: Kleisli[F, A, A] = Kleisli.ask[F, A]

  def local[B](f: A => A)(fa: Kleisli[F, A, B]): Kleisli[F, A, B] = Kleisli.local(f)(fa)
}

private trait KleisliMonad[F[_], A] extends Monad[Kleisli[F, A, ?]] with KleisliFlatMap[F, A] with KleisliApplicative[F, A] {
  implicit def F: Monad[F]
}

private trait KleisliFlatMap[F[_], A] extends FlatMap[Kleisli[F, A, ?]] with KleisliApply[F, A] {
  implicit def F: FlatMap[F]

  def flatMap[B, C](fa: Kleisli[F, A, B])(f: B => Kleisli[F, A, C]): Kleisli[F, A, C] =
    fa.flatMap(f)

  def tailRecM[B, C](b: B)(f: B => Kleisli[F, A, Either[B, C]]): Kleisli[F, A, C] =
    Kleisli[F, A, C]({ a => F.tailRecM(b) { f(_).run(a) } })
}

private trait KleisliApplicative[F[_], A] extends Applicative[Kleisli[F, A, ?]] with KleisliApply[F, A] {
  implicit def F: Applicative[F]

  def pure[B](x: B): Kleisli[F, A, B] =
    Kleisli.pure[F, A, B](x)
}

private trait KleisliApply[F[_], A] extends Apply[Kleisli[F, A, ?]] with KleisliFunctor[F, A] {
  implicit def F: Apply[F]

  override def ap[B, C](f: Kleisli[F, A, B => C])(fa: Kleisli[F, A, B]): Kleisli[F, A, C] =
    fa.ap(f)

  override def product[B, C](fb: Kleisli[F, A, B], fc: Kleisli[F, A, C]): Kleisli[F, A, (B, C)] =
    Kleisli(a => F.product(fb.run(a), fc.run(a)))
}

private trait KleisliFunctor[F[_], A] extends Functor[Kleisli[F, A, ?]] {
  implicit def F: Functor[F]

  override def map[B, C](fa: Kleisli[F, A, B])(f: B => C): Kleisli[F, A, C] =
    fa.map(f)
}
