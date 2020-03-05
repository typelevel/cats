package cats
package data

import cats.{Contravariant, Id}
import cats.arrow._

/**
 * Represents a function `A => F[B]`.
 */
final case class Kleisli[F[_], -A, B](run: A => F[B]) { self =>

  def ap[C, AA <: A](f: Kleisli[F, AA, B => C])(implicit F: Apply[F]): Kleisli[F, AA, C] =
    Kleisli(a => F.ap(f.run(a))(run(a)))

  /**
   * Performs [[local]] and [[map]] simultaneously.
   */
  def dimap[C, D](f: C => A)(g: B => D)(implicit F: Functor[F]): Kleisli[F, C, D] =
    Kleisli(c => F.map(run(f(c)))(g))

  /**
   * Modify the output of the Kleisli function with `f`.
   * {{{
   * scala> import cats.data.Kleisli, cats.implicits._
   * scala> val takeHead = Kleisli[Option, List[Int], Int](_.headOption)
   * scala> takeHead.map(_.toDouble).run(List(1))
   * res0: Option[Double] = Some(1.0)
   * }}}
   */
  def map[C](f: B => C)(implicit F: Functor[F]): Kleisli[F, A, C] =
    Kleisli(a => F.map(run(a))(f))

  def mapF[N[_], C](f: F[B] => N[C]): Kleisli[N, A, C] =
    Kleisli(a => f(run(a)))

  /**
   * Modify the context `F` using transformation `f`.
   */
  def mapK[G[_]](f: F ~> G): Kleisli[G, A, B] =
    Kleisli[G, A, B](a => f(run(a)))

  def flatMap[C, AA <: A](f: B => Kleisli[F, AA, C])(implicit F: FlatMap[F]): Kleisli[F, AA, C] =
    Kleisli.shift(a => F.flatMap[B, C](run(a))((b: B) => f(b).run(a)))

  def flatMapF[C](f: B => F[C])(implicit F: FlatMap[F]): Kleisli[F, A, C] =
    Kleisli.shift(a => F.flatMap(run(a))(f))

  /**
   * Composes [[run]] with a function `B => F[C]` not lifted into Kleisli.
   */
  def andThen[C](f: B => F[C])(implicit F: FlatMap[F]): Kleisli[F, A, C] =
    Kleisli.shift(a => F.flatMap(run(a))(f))

  /**
   * Tip to tail Kleisli arrow composition.
   * Creates a function `A => F[C]` from [[run]] (`A => F[B]`) and the given Kleisli of `B => F[C]`.
   * {{{
   * scala> import cats.data.Kleisli, cats.implicits._
   * scala> val takeHead = Kleisli[Option, List[Int], Int](_.headOption)
   * scala> val plusOne = Kleisli[Option, Int, Int](i => Some(i + 1))
   * scala> (takeHead andThen plusOne).run(List(1))
   * res0: Option[Int] = Some(2)
   * }}}
   */
  def andThen[C](k: Kleisli[F, B, C])(implicit F: FlatMap[F]): Kleisli[F, A, C] =
    this.andThen(k.run)

  def compose[Z, AA <: A](f: Z => F[AA])(implicit F: FlatMap[F]): Kleisli[F, Z, B] =
    Kleisli.shift((z: Z) => F.flatMap(f(z))(run))

  def compose[Z, AA <: A](k: Kleisli[F, Z, AA])(implicit F: FlatMap[F]): Kleisli[F, Z, B] =
    this.compose(k.run)

  def traverse[G[_], AA <: A](f: G[AA])(implicit F: Applicative[F], G: Traverse[G]): F[G[B]] =
    G.traverse(f)(run)

  def lift[G[_]](implicit G: Applicative[G]): Kleisli[λ[α => G[F[α]]], A, B] =
    Kleisli[λ[α => G[F[α]]], A, B](a => Applicative[G].pure(run(a)))

  /**
   * Contramap the input using `f`, where `f` may modify the input type of the Kleisli arrow.
   * {{{
   * scala> import cats.data.Kleisli, cats.implicits._
   * scala> type ParseResult[A] = Either[Throwable, A]
   * scala> val parseInt = Kleisli[ParseResult, String, Int](s => Either.catchNonFatal(s.toInt))
   * scala> parseInt.local[List[String]](_.combineAll).run(List("1", "2"))
   * res0: ParseResult[Int] = Right(12)
   * }}}
   */
  def local[AA](f: AA => A): Kleisli[F, AA, B] =
    Kleisli(aa => run(f(aa)))

  @deprecated("Use mapK", "1.0.0-RC2")
  private[cats] def transform[G[_]](f: FunctionK[F, G]): Kleisli[G, A, B] =
    mapK(f)

  def lower(implicit F: Applicative[F]): Kleisli[F, A, F[B]] =
    Kleisli(a => F.pure(run(a)))

  def first[C](implicit F: Functor[F]): Kleisli[F, (A, C), (B, C)] =
    Kleisli { case (a, c) => F.fproduct(run(a))(_ => c) }

  def second[C](implicit F: Functor[F]): Kleisli[F, (C, A), (C, B)] =
    Kleisli { case (c, a) => F.map(run(a))(c -> _) }

  /** Discard computed B and yield the input value. */
  def tap[AA <: A](implicit F: Functor[F]): Kleisli[F, AA, AA] =
    Kleisli(a => F.as(run(a), a))

  /** Yield computed B combined with input value. */
  def tapWith[C, AA <: A](f: (AA, B) => C)(implicit F: Functor[F]): Kleisli[F, AA, C] =
    Kleisli(a => F.map(run(a))(b => f(a, b)))

  def tapWithF[C, AA <: A](f: (AA, B) => F[C])(implicit F: FlatMap[F]): Kleisli[F, AA, C] =
    Kleisli(a => F.flatMap(run(a))(b => f(a, b)))

  def toReader: Reader[A, F[B]] = Kleisli[Id, A, F[B]](run)

  def apply(a: A): F[B] = run(a)
}

object Kleisli
    extends KleisliInstances
    with KleisliFunctions
    with KleisliFunctionsBinCompat
    with KleisliExplicitInstances {

  /**
   * Internal API — shifts the execution of `run` in the `F` context.
   *
   * Used to build Kleisli values for `F[_]` data types that implement `Monad`,
   * in which case it is safer to trigger the `F[_]` context earlier.
   *
   * The requirement is for `FlatMap` as this will get used in operations
   * that invoke `F.flatMap` (e.g. in `Kleisli#flatMap`). However we are
   * doing discrimination based on inheritance and if we detect an
   * `Applicative`, then we use it to trigger the `F[_]` context earlier.
   *
   * Triggering the `F[_]` context earlier is important to avoid stack
   * safety issues for `F` monads that have a stack safe `flatMap`
   * implementation. For example `Eval` or `IO`. Without this the `Monad`
   * instance is stack unsafe, even if the underlying `F` is stack safe
   * in `flatMap`.
   */
  private[data] def shift[F[_], A, B](run: A => F[B])(implicit F: FlatMap[F]): Kleisli[F, A, B] =
    F match {
      case ap: Applicative[F] @unchecked =>
        Kleisli(r => F.flatMap(ap.pure(r))(run))
      case _ =>
        Kleisli(run)
    }

  /**
   * Creates a `FunctionK` that transforms a `Kleisli[F, A, B]` into an `F[B]` by applying the value of type `a:A`.
   * {{{
   * scala> import cats.{~>}, cats.data.{Kleisli, EitherT}
   *
   * scala> def f(i: Int): Option[Either[Char, Char]] = if (i > 0) Some(Right('n')) else if (i < 0) Some(Left('z')) else None
   *
   * scala> type KOI[A] = Kleisli[Option, Int, A]
   * scala> val b: KOI[Either[Char, Char]] = Kleisli[Option, Int, Either[Char, Char]](f _)
   * scala> val nt: Kleisli[Option, Int, *] ~> Option = Kleisli.applyK[Option, Int](1)
   * scala> nt(b)
   * res0: Option[Either[Char, Char]] = Some(Right(n))
   *
   * scala> type EKOIC[A] = EitherT[KOI, Char, A]
   * scala> val c: EKOIC[Char] = EitherT[KOI, Char, Char](b)
   * scala> c.mapK(nt).value
   * res1: Option[Either[Char, Char]] = Some(Right(n))
   *
   * scala> val ntz = Kleisli.applyK[Option, Int](0)
   * scala> c.mapK(ntz).value
   * res2: Option[Either[Char, Char]] = None
   * }}}
   */
  def applyK[F[_], A](a: A): Kleisli[F, A, *] ~> F =
    λ[Kleisli[F, A, *] ~> F](_.apply(a))

}

sealed private[data] trait KleisliFunctions {

  /**
   * Creates a Kleisli that ignores its input `A` and returns the given `F[B]`.
   * {{{
   * scala> import cats.data.Kleisli, cats.implicits._
   * scala> val takeHead = Kleisli((_:List[Int]).headOption)
   * scala> val makeList = Kleisli.liftF[Option, Unit, List[Int]](Some(List(1,2,3)))
   * scala> (makeList andThen takeHead).run(())
   * res0: Option[Int] = Some(1)
   * }}}
   */
  def liftF[F[_], A, B](x: F[B]): Kleisli[F, A, B] =
    Kleisli(_ => x)

  /**
   * Same as [[liftF]], but expressed as a FunctionK for use with mapK
   * {{{
   * scala> import cats._, data._, implicits._
   * scala> val a: OptionT[Eval, Int] = 1.pure[OptionT[Eval, *]]
   * scala> val b: OptionT[Kleisli[Eval, String, *], Int] = a.mapK(Kleisli.liftK)
   * scala> b.value.run("").value
   * res0: Option[Int] = Some(1)
   * }}}
   */
  def liftK[F[_], A]: F ~> Kleisli[F, A, *] =
    λ[F ~> Kleisli[F, A, *]](Kleisli.liftF(_))

  @deprecated("Use liftF instead", "1.0.0-RC2")
  private[cats] def lift[F[_], A, B](x: F[B]): Kleisli[F, A, B] =
    Kleisli(_ => x)

  /**
   * Creates a Kleisli arrow ignoring its input and lifting the given `B` into applicative context `F`.
   * {{{
   * scala> import cats.data.Kleisli, cats.implicits._
   * scala> val pureOpt = Kleisli.pure[Option, Unit, String]("beam me up!")
   * scala> pureOpt.run(())
   * res0: Option[String] = Some(beam me up!)
   * }}}
   */
  def pure[F[_], A, B](x: B)(implicit F: Applicative[F]): Kleisli[F, A, B] =
    Kleisli(_ => F.pure(x))

  /**
   * Creates a Kleisli arrow which can lift an `A` into applicative context `F`.
   * This is distinct from [[pure]] in that the input is what is lifted (and not ignored).
   * {{{
   * scala> Kleisli.ask[Option, Int].run(1)
   * res0: Option[Int]: Some(1)
   * }}}
   */
  def ask[F[_], A](implicit F: Applicative[F]): Kleisli[F, A, A] =
    Kleisli(F.pure)

  /**
   * Modifies the input environment with `f`, without changing the input type of the Kleisli.
   * {{{
   * scala> import cats.data.Kleisli
   * scala> val takeHead = Kleisli[Option, List[Int], Int](_.headOption)
   * scala> Kleisli.local[Option, Int, List[Int]](1 :: _)(takeHead).run(List(2,3))
   * res0: Option[Int] = Some(1)
   * }}}
   */
  def local[M[_], A, R](f: R => R)(fa: Kleisli[M, R, A]): Kleisli[M, R, A] =
    Kleisli(r => fa.run(f(r)))
}

sealed private[data] trait KleisliFunctionsBinCompat {

  /**
   * Lifts a natural transformation of effects within a Kleisli
   * to a transformation of Kleislis.
   *
   * Equivalent to running `mapK(f) on a Kleisli.
   *
   * {{{
   * scala> import cats._, data._
   * scala> val f: (List ~> Option) = λ[List ~> Option](_.headOption)
   *
   * scala> val k: Kleisli[List, String, Char] = Kleisli(_.toList)
   * scala> k.run("foo")
   * res0: List[Char] = List(f, o, o)
   *
   * scala> val k2: Kleisli[Option, String, Char] = Kleisli.liftFunctionK(f)(k)
   * scala> k2.run("foo")
   * res1: Option[Char] = Some(f)
   * }}}
   * */
  def liftFunctionK[F[_], G[_], A](f: F ~> G): Kleisli[F, A, *] ~> Kleisli[G, A, *] =
    λ[Kleisli[F, A, *] ~> Kleisli[G, A, *]](_.mapK(f))
}

sealed private[data] trait KleisliExplicitInstances {

  def endoSemigroupK[F[_]](implicit FM: FlatMap[F]): SemigroupK[λ[α => Kleisli[F, α, α]]] =
    Compose[Kleisli[F, *, *]].algebraK

  def endoMonoidK[F[_]](implicit M: Monad[F]): MonoidK[λ[α => Kleisli[F, α, α]]] =
    Category[Kleisli[F, *, *]].algebraK
}

sealed abstract private[data] class KleisliInstances extends KleisliInstances0 {
  implicit def catsDataMonadForKleisliId[A]: CommutativeMonad[Kleisli[Id, A, *]] =
    catsDataCommutativeMonadForKleisli[Id, A]

  implicit val catsDataCommutativeArrowForKleisliId: CommutativeArrow[Kleisli[Id, *, *]] =
    catsDataCommutativeArrowForKleisli[Id]

  implicit def catsDataDeferForKleisli[F[_], A](implicit F: Defer[F]): Defer[Kleisli[F, A, *]] =
    new Defer[Kleisli[F, A, *]] {
      def defer[B](fa: => Kleisli[F, A, B]): Kleisli[F, A, B] = {
        lazy val cacheFa = fa
        Kleisli[F, A, B] { a =>
          F.defer(cacheFa.run(a))
        }
      }
    }

  implicit def catsDataFunctorFilterForKleisli[F[_], A](
    implicit ev: FunctorFilter[F]
  ): FunctorFilter[Kleisli[F, A, *]] =
    new KleisliFunctorFilter[F, A] { val FF = ev }
}

sealed abstract private[data] class KleisliInstances0 extends KleisliInstances0_5 {

  implicit def catsDataCommutativeArrowForKleisli[F[_]](
    implicit M: CommutativeMonad[F]
  ): CommutativeArrow[Kleisli[F, *, *]] with ArrowChoice[Kleisli[F, *, *]] =
    new KleisliCommutativeArrow[F] { def F: CommutativeMonad[F] = M }

  implicit def catsDataCommutativeMonadForKleisli[F[_], A](
    implicit F0: CommutativeMonad[F]
  ): CommutativeMonad[Kleisli[F, A, *]] =
    new KleisliMonad[F, A] with CommutativeMonad[Kleisli[F, A, *]] {
      implicit def F: Monad[F] = F0
    }

}

sealed abstract private[data] class KleisliInstances0_5 extends KleisliInstances1 {
  implicit def catsDataMonoidForKleisli[F[_], A, B](implicit FB0: Monoid[F[B]]): Monoid[Kleisli[F, A, B]] =
    new KleisliMonoid[F, A, B] { def FB: Monoid[F[B]] = FB0 }

  implicit def catsDataMonadErrorForKleisli[F[_], A, E](
    implicit ME: MonadError[F, E]
  ): MonadError[Kleisli[F, A, *], E] =
    new KleisliMonadError[F, A, E] { def F: MonadError[F, E] = ME }

  implicit def catsDataArrowChoiceForKleisli[F[_]](implicit M: Monad[F]): ArrowChoice[Kleisli[F, *, *]] =
    new KleisliArrowChoice[F] {
      def F: Monad[F] = M
    }

  implicit def catsDataContravariantMonoidalForKleisli[F[_], A](
    implicit F0: ContravariantMonoidal[F]
  ): ContravariantMonoidal[Kleisli[F, A, *]] =
    new KleisliContravariantMonoidal[F, A] { def F: ContravariantMonoidal[F] = F0 }

  /**
   * Witness for: Kleisli[M, E, A] <-> (E, R) => A
   * if M is Representable
   */
  implicit def catsDataRepresentableForKleisli[M[_], R, E](
    implicit
    R: Representable.Aux[M, R],
    FK: Functor[Kleisli[M, E, *]]
  ): Representable.Aux[Kleisli[M, E, *], (E, R)] = new Representable[Kleisli[M, E, *]] {

    override type Representation = (E, R)

    override val F: Functor[Kleisli[M, E, *]] = FK

    def index[A](f: Kleisli[M, E, A]): Representation => A = {
      case (e, r) => R.index(f.run(e))(r)
    }

    def tabulate[A](f: Representation => A): Kleisli[M, E, A] =
      Kleisli[M, E, A](e => R.tabulate(r => f((e, r))))
  }
}

sealed abstract private[data] class KleisliInstances1 extends KleisliInstances2 {
  implicit def catsDataMonadForKleisli[F[_], A](implicit M: Monad[F]): Monad[Kleisli[F, A, *]] =
    new KleisliMonad[F, A] { def F: Monad[F] = M }

  implicit def catsDataParallelForKleisli[M[_], A](
    implicit P: Parallel[M]
  ): Parallel.Aux[Kleisli[M, A, *], Kleisli[P.F, A, *]] = new Parallel[Kleisli[M, A, *]] {
    type F[x] = Kleisli[P.F, A, x]
    implicit val monadM: Monad[M] = P.monad
    def applicative: Applicative[Kleisli[P.F, A, *]] = catsDataApplicativeForKleisli(P.applicative)
    def monad: Monad[Kleisli[M, A, *]] = catsDataMonadForKleisli

    def sequential: Kleisli[P.F, A, *] ~> Kleisli[M, A, *] =
      λ[Kleisli[P.F, A, *] ~> Kleisli[M, A, *]](_.mapK(P.sequential))

    def parallel: Kleisli[M, A, *] ~> Kleisli[P.F, A, *] =
      λ[Kleisli[M, A, *] ~> Kleisli[P.F, A, *]](_.mapK(P.parallel))
  }

  implicit def catsDataContravariantForKleisli[F[_], C]: Contravariant[Kleisli[F, *, C]] =
    new Contravariant[Kleisli[F, *, C]] {
      override def contramap[A, B](fa: Kleisli[F, A, C])(f: B => A): Kleisli[F, B, C] =
        fa.local(f)
    }
}

sealed abstract private[data] class KleisliInstances2 extends KleisliInstances3 {
  implicit def catsDataAlternativeForKleisli[F[_], A](implicit F0: Alternative[F]): Alternative[Kleisli[F, A, *]] =
    new KleisliAlternative[F, A] { def F: Alternative[F] = F0 }
}

sealed abstract private[data] class KleisliInstances3 extends KleisliInstances4 {
  implicit def catsDataMonoidKForKleisli[F[_], A](implicit F0: MonoidK[F]): MonoidK[Kleisli[F, A, *]] =
    new KleisliMonoidK[F, A] { def F: MonoidK[F] = F0 }

  implicit def catsDataCommutativeFlatMapForKleisli[F[_], A](
    implicit F0: CommutativeFlatMap[F]
  ): CommutativeFlatMap[Kleisli[F, A, *]] =
    new KleisliFlatMap[F, A] with CommutativeFlatMap[Kleisli[F, A, *]] { val F: CommutativeFlatMap[F] = F0 }

  implicit def catsDataChoiceForKleisli[F[_]](implicit M: Monad[F]): Choice[Kleisli[F, *, *]] =
    new KleisliChoice[F] { def F: Monad[F] = M }

  implicit val catsDataChoiceForKleisliId: Choice[Kleisli[Id, *, *]] =
    catsDataChoiceForKleisli[Id]

  implicit def catsDataComposeForKleisli[F[_]](implicit FM: FlatMap[F]): Compose[Kleisli[F, *, *]] =
    new KleisliCompose[F] { def F: FlatMap[F] = FM }

  implicit def catsDataStrongForKleisli[F[_]](implicit F0: Functor[F]): Strong[Kleisli[F, *, *]] =
    new KleisliStrong[F] { def F: Functor[F] = F0 }

  implicit def catsDataSemigroupForKleisli[F[_], A, B](implicit FB0: Semigroup[F[B]]): Semigroup[Kleisli[F, A, B]] =
    new KleisliSemigroup[F, A, B] { def FB: Semigroup[F[B]] = FB0 }
}

sealed abstract private[data] class KleisliInstances4 extends KleisliInstances5 {
  implicit def catsDataSemigroupKForKleisli[F[_], A](implicit F0: SemigroupK[F]): SemigroupK[Kleisli[F, A, *]] =
    new KleisliSemigroupK[F, A] { def F: SemigroupK[F] = F0 }

  implicit def catsDataFlatMapForKleisli[F[_], A](implicit FM: FlatMap[F]): FlatMap[Kleisli[F, A, *]] =
    new KleisliFlatMap[F, A] { def F: FlatMap[F] = FM }

}

sealed abstract private[data] class KleisliInstances5 extends KleisliInstances6 {

  implicit def catsDataApplicativeErrorForKleisli[F[_], E, A](
    implicit F0: ApplicativeError[F, E]
  ): ApplicativeError[Kleisli[F, A, *], E] =
    new KleisliApplicativeError[F, A, E] { def F: ApplicativeError[F, E] = F0 }
}

sealed abstract private[data] class KleisliInstances6 extends KleisliInstances7 {
  implicit def catsDataApplicativeForKleisli[F[_], A](implicit A: Applicative[F]): Applicative[Kleisli[F, A, *]] =
    new KleisliApplicative[F, A] { def F: Applicative[F] = A }
}

sealed abstract private[data] class KleisliInstances7 extends KleisliInstances8 {
  implicit def catsDataApplyForKleisli[F[_], A](implicit A: Apply[F]): Apply[Kleisli[F, A, *]] =
    new KleisliApply[F, A] { def F: Apply[F] = A }
}

sealed abstract private[data] class KleisliInstances8 extends KleisliInstances9 {
  implicit def catsDataDistributiveForKleisli[F[_], R](implicit F0: Distributive[F]): Distributive[Kleisli[F, R, *]] =
    new KleisliDistributive[F, R] with KleisliFunctor[F, R] { implicit def F: Distributive[F] = F0 }
}

sealed abstract private[data] class KleisliInstances9 {
  implicit def catsDataFunctorForKleisli[F[_], A](implicit F0: Functor[F]): Functor[Kleisli[F, A, *]] =
    new KleisliFunctor[F, A] { def F: Functor[F] = F0 }
}

private[data] trait KleisliCommutativeArrow[F[_]]
    extends CommutativeArrow[Kleisli[F, *, *]]
    with KleisliArrowChoice[F] {
  implicit def F: CommutativeMonad[F]
}

private[data] trait KleisliArrowChoice[F[_]]
    extends ArrowChoice[Kleisli[F, *, *]]
    with KleisliCategory[F]
    with KleisliStrong[F] {
  implicit def F: Monad[F]

  def lift[A, B](f: A => B): Kleisli[F, A, B] =
    Kleisli(a => F.pure(f(a)))

  override def split[A, B, C, D](f: Kleisli[F, A, B], g: Kleisli[F, C, D]): Kleisli[F, (A, C), (B, D)] =
    Kleisli { case (a, c) => F.flatMap(f.run(a))(b => F.map(g.run(c))(d => (b, d))) }

  def choose[A, B, C, D](f: Kleisli[F, A, C])(g: Kleisli[F, B, D]): Kleisli[F, Either[A, B], Either[C, D]] =
    Kleisli((fe: Either[A, B]) =>
      fe match {
        case Left(a)  => F.map(f(a))(Left.apply _)
        case Right(b) => F.map(g(b))(Right.apply _)
      }
    )
}

private[data] trait KleisliStrong[F[_]] extends Strong[Kleisli[F, *, *]] {
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

private[data] trait KleisliChoice[F[_]] extends Choice[Kleisli[F, *, *]] with KleisliCategory[F] {
  def choice[A, B, C](f: Kleisli[F, A, C], g: Kleisli[F, B, C]): Kleisli[F, Either[A, B], C] =
    Kleisli(_.fold(f.run, g.run))
}

private[data] trait KleisliCategory[F[_]] extends Category[Kleisli[F, *, *]] with KleisliCompose[F] {
  implicit def F: Monad[F]

  override def id[A]: Kleisli[F, A, A] = Kleisli.ask[F, A]
}

private[data] trait KleisliCompose[F[_]] extends Compose[Kleisli[F, *, *]] {
  implicit def F: FlatMap[F]

  def compose[A, B, C](f: Kleisli[F, B, C], g: Kleisli[F, A, B]): Kleisli[F, A, C] =
    f.compose(g)
}

private[data] trait KleisliSemigroup[F[_], A, B] extends Semigroup[Kleisli[F, A, B]] {
  implicit def FB: Semigroup[F[B]]

  override def combine(a: Kleisli[F, A, B], b: Kleisli[F, A, B]): Kleisli[F, A, B] =
    Kleisli[F, A, B](x => FB.combine(a.run(x), b.run(x)))
}

private[data] trait KleisliMonoid[F[_], A, B] extends Monoid[Kleisli[F, A, B]] with KleisliSemigroup[F, A, B] {
  implicit def FB: Monoid[F[B]]

  override def empty: Kleisli[F, A, B] = Kleisli[F, A, B](_ => FB.empty)
}

sealed private[data] trait KleisliSemigroupK[F[_], A] extends SemigroupK[Kleisli[F, A, *]] {
  implicit def F: SemigroupK[F]

  override def combineK[B](x: Kleisli[F, A, B], y: Kleisli[F, A, B]): Kleisli[F, A, B] =
    Kleisli(a => F.combineK(x.run(a), y.run(a)))
}

sealed private[data] trait KleisliMonoidK[F[_], A] extends MonoidK[Kleisli[F, A, *]] with KleisliSemigroupK[F, A] {
  implicit def F: MonoidK[F]

  override def empty[B]: Kleisli[F, A, B] = Kleisli.liftF(F.empty[B])
}

private[data] trait KleisliAlternative[F[_], A]
    extends Alternative[Kleisli[F, A, *]]
    with KleisliApplicative[F, A]
    with KleisliMonoidK[F, A] {
  implicit def F: Alternative[F]
}

sealed private[data] trait KleisliContravariantMonoidal[F[_], D] extends ContravariantMonoidal[Kleisli[F, D, *]] {
  implicit def F: ContravariantMonoidal[F]

  override def unit: Kleisli[F, D, Unit] = Kleisli(Function.const(F.unit))

  override def contramap[A, B](fa: Kleisli[F, D, A])(f: B => A): Kleisli[F, D, B] =
    Kleisli(d => F.contramap(fa.run(d))(f))

  override def product[A, B](fa: Kleisli[F, D, A], fb: Kleisli[F, D, B]): Kleisli[F, D, (A, B)] =
    Kleisli(d => F.product(fa.run(d), fb.run(d)))
}

private[data] trait KleisliMonadError[F[_], A, E]
    extends MonadError[Kleisli[F, A, *], E]
    with KleisliApplicativeError[F, A, E]
    with KleisliMonad[F, A] {
  def F: MonadError[F, E]
}

private[data] trait KleisliApplicativeError[F[_], A, E]
    extends ApplicativeError[Kleisli[F, A, *], E]
    with KleisliApplicative[F, A] {
  type K[T] = Kleisli[F, A, T]

  implicit def F: ApplicativeError[F, E]

  def raiseError[B](e: E): K[B] = Kleisli(_ => F.raiseError(e))

  def handleErrorWith[B](kb: K[B])(f: E => K[B]): K[B] = Kleisli { (a: A) =>
    F.handleErrorWith(kb.run(a))((e: E) => f(e).run(a))
  }
}

private[data] trait KleisliMonad[F[_], A]
    extends Monad[Kleisli[F, A, *]]
    with KleisliFlatMap[F, A]
    with KleisliApplicative[F, A] {
  implicit def F: Monad[F]
}

private[data] trait KleisliFlatMap[F[_], A] extends FlatMap[Kleisli[F, A, *]] with KleisliApply[F, A] {
  implicit def F: FlatMap[F]

  def flatMap[B, C](fa: Kleisli[F, A, B])(f: B => Kleisli[F, A, C]): Kleisli[F, A, C] =
    fa.flatMap(f)

  def tailRecM[B, C](b: B)(f: B => Kleisli[F, A, Either[B, C]]): Kleisli[F, A, C] =
    Kleisli[F, A, C] { a =>
      F.tailRecM(b)(f(_).run(a))
    }
}

private[data] trait KleisliApplicative[F[_], A] extends Applicative[Kleisli[F, A, *]] with KleisliApply[F, A] {
  implicit def F: Applicative[F]

  def pure[B](x: B): Kleisli[F, A, B] =
    Kleisli.pure[F, A, B](x)
}

private[data] trait KleisliApply[F[_], A] extends Apply[Kleisli[F, A, *]] with KleisliFunctor[F, A] {
  implicit def F: Apply[F]

  override def ap[B, C](f: Kleisli[F, A, B => C])(fa: Kleisli[F, A, B]): Kleisli[F, A, C] =
    fa.ap(f)

  override def product[B, C](fb: Kleisli[F, A, B], fc: Kleisli[F, A, C]): Kleisli[F, A, (B, C)] =
    Kleisli(a => F.product(fb.run(a), fc.run(a)))
}

private[data] trait KleisliFunctor[F[_], A] extends Functor[Kleisli[F, A, *]] {
  implicit def F: Functor[F]

  override def map[B, C](fa: Kleisli[F, A, B])(f: B => C): Kleisli[F, A, C] =
    fa.map(f)
}

private trait KleisliDistributive[F[_], R] extends Distributive[Kleisli[F, R, *]] {
  implicit def F: Distributive[F]

  override def distribute[G[_]: Functor, A, B](a: G[A])(f: A => Kleisli[F, R, B]): Kleisli[F, R, G[B]] =
    Kleisli(r => F.distribute(a)(f(_).run(r)))

  def map[A, B](fa: Kleisli[F, R, A])(f: A => B): Kleisli[F, R, B] = fa.map(f)
}

private[this] trait KleisliFunctorFilter[F[_], R] extends FunctorFilter[Kleisli[F, R, *]] {

  def FF: FunctorFilter[F]

  def functor: Functor[Kleisli[F, R, *]] = Kleisli.catsDataFunctorForKleisli(FF.functor)

  def mapFilter[A, B](fa: Kleisli[F, R, A])(f: A => Option[B]): Kleisli[F, R, B] =
    Kleisli[F, R, B] { r =>
      FF.mapFilter(fa.run(r))(f)
    }
}
