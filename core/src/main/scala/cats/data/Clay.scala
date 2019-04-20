package cats
package data

import cats.{Contravariant, Id}
import cats.arrow._

/**
 * A Clay represents a function `A => F[B]`, but provides an interface whose type symbol exposes the `F[_]` parameter.
 */
trait Clay[F[_], A, B] { self =>

  def apply(a: A): F[B]

  def ap[C](f: Clay[F, A, B => C])(implicit F: Apply[F]): Clay[F, A, C] =
    new Clay[F, A, C] { def apply(a: A): F[C] = F.ap(f(a))(self(a)) }

  /**
   * Performs [[local]] and [[map]] simultaneously.
   */
  def dimap[C, D](f: C => A)(g: B => D)(implicit F: Functor[F]): Clay[F, C, D] =
    new Clay[F, C, D] { def apply(c: C): F[D] = F.map(self(f(c)))(g) }

  /**
   * Modify the output of the Clay function with `f`.
   * {{{
   * scala> import cats.data.Clay, cats.implicits._
   * scala> val takeHead = Clay[Option, List[Int], Int](_.headOption)
   * scala> takeHead.map(_.toDouble).apply(List(1))
   * res0: Option[Double] = Some(1.0)
   * }}}
   */
  def map[C](f: B => C)(implicit F: Functor[F]): Clay[F, A, C] =
    new Clay[F, A, C] { def apply(a: A): F[C] = F.map(self(a))(f) }

  def mapF[N[_], C](f: F[B] => N[C]): Clay[N, A, C] =
    new Clay[N, A, C] { def apply(a: A): N[C] = f(self(a)) }

  /**
   * Modify the context `F` using transformation `f`.
   */
  def mapK[G[_]](f: F ~> G): Clay[G, A, B] =
    new Clay[G, A, B] { def apply(a: A): G[B] = f(self(a)) }

  /**
   * Tip to tail Clay arrow composition.
   * Creates a function `A => F[C]` from [[apply]] (`A => F[B]`) and the given Clay of `B => F[C]`.
   * {{{
   * scala> import cats.data.Clay, cats.implicits._
   * scala> val takeHead = Clay[Option, List[Int], Int](_.headOption)
   * scala> val plusOne  = Clay[Option, Int, Int](i => Some(i + 1))
   * scala> (takeHead andThen plusOne).apply(List(1))
   * res0: Option[Int] = Some(2)
   * }}}
   */
  def andThen[C](k: Clay[F, B, C])(implicit F: FlatMap[F]): Clay[F, A, C] =
    new Clay[F, A, C] { def apply(a: A) = F.flatMap(self(a))(k.apply) }

  def flatMap[C](f: B => Clay[F, A, C])(implicit F: FlatMap[F]): Clay[F, A, C] =
    Clay.shift { a =>
      F.flatMap[B, C](self(a))((b: B) => f(b)(a))
    }

  def flatMapF[C](f: B => F[C])(implicit F: FlatMap[F]): Clay[F, A, C] =
    Clay.shift { a =>
      F.flatMap(self(a))(f)
    }

  /**
   * Contramap the input using `f`, where `f` may modify the input type of the Clay arrow.
   * {{{
   * scala> import cats.data.Clay, cats.implicits._
   * scala> type ParseResult[A] = Either[Throwable, A]
   * scala> val parseInt: Clay[ParseResult, String, Int] = Clay(s => Either.catchNonFatal(s.toInt))
   * scala> parseInt.local[List[String]](_.combineAll).apply(List("1", "2"))
   * res0: ParseResult[Int] = Right(12)
   * }}}
   */
  def local[AA](f: AA => A): Clay[F, AA, B] = // SAM: aa => self(f(aa))
    new Clay[F, AA, B] { def apply(aa: AA): F[B] = self(f(aa)) }

  def lower(implicit F: Applicative[F]): Clay[F, A, F[B]] =
    new Clay[F, A, F[B]] { def apply(a: A): F[F[B]] = F.pure(self(a)) }

  def first[C](implicit F: Functor[F]): Clay[F, (A, C), (B, C)] =
    new Clay[F, (A, C), (B, C)] {
      def apply(ac: (A, C)): F[(B, C)] = F.fproduct(self(ac._1))(_ => ac._2)
    }

  def second[C](implicit F: Functor[F]): Clay[F, (C, A), (C, B)] =
    new Clay[F, (C, A), (C, B)] {
      def apply(ca: (C, A)): F[(C, B)] = F.map(self(ca._2))(ca._1 -> _)
    }

  def merge[C](g: Clay[F, A, C])(implicit F: Apply[F]): Clay[F, A, (B, C)] =
    new Clay[F, A, (B, C)] { def apply(a: A): F[(B, C)] = F.product(self(a), g(a)) }

  /** Discard computed B and yield the input value. */
  def tap(implicit F: Functor[F]): Clay[F, A, A] =
    new Clay[F, A, A] { def apply(a: A): F[A] = F.as(self(a), a) }

  /** Yield computed B combined with input value. */
  def tapWith[C](f: (A, B) => C)(implicit F: Functor[F]): Clay[F, A, C] =
    new Clay[F, A, C] { def apply(a: A): F[C] = F.map(self(a))(b => f(a, b)) }

  def tapWithF[C](f: (A, B) => F[C])(implicit F: FlatMap[F]): Clay[F, A, C] =
    new Clay[F, A, C] { def apply(a: A): F[C] = F.flatMap(self(a))(b => f(a, b)) }

  /**
   * Composes [[run]] with a function `B => F[C]` not lifted into Clay.
   */
  def andThen[C](f: B => F[C])(implicit F: FlatMap[F]): Clay[F, A, C] =
    Clay.shift(a => F.flatMap(self(a))(f))

  def compose[Z](f: Z => F[A])(implicit F: FlatMap[F]): Clay[F, Z, B] =
    Clay.shift((z: Z) => F.flatMap(f(z))(self.apply))

  def compose[Z](k: Clay[F, Z, A])(implicit F: FlatMap[F]): Clay[F, Z, B] =
    self.compose(k.apply(_))

  def traverse[G[_]](f: G[A])(implicit F: Applicative[F], G: Traverse[G]): F[G[B]] =
    G.traverse(f)(apply)

  def lift[G[_]](implicit G: Applicative[G]): Clay[λ[α => G[F[α]]], A, B] =
    new Clay[λ[α => G[F[α]]], A, B] { def apply(a: A): G[F[B]] = G.pure(self(a)) }
}

object Clay extends ClayInstances with ClayFunctions with ClayFunctionsBinCompat with ClayExplicitInstances {

  def apply[F[_], A, B](ff: A => F[B]): Clay[F, A, B] =
    new Clay[F, A, B] { def apply(a: A): F[B] = ff(a) }

  /**
   * Internal API — shifts the execution of `run` in the `F` context.
   *
   * Used to build Clay values for `F[_]` data types that implement `Monad`,
   * in which case it is safer to trigger the `F[_]` context earlier.
   *
   * The requirement is for `FlatMap` as this will get used in operations
   * that invoke `F.flatMap` (e.g. in `Clay#flatMap`). However we are
   * doing discrimination based on inheritance and if we detect an
   * `Applicative`, then we use it to trigger the `F[_]` context earlier.
   *
   * Triggering the `F[_]` context earlier is important to avoid stack
   * safety issues for `F` monads that have a stack safe `flatMap`
   * implementation. For example `Eval` or `IO`. Without this the `Monad`
   * instance is stack unsafe, even if the underlying `F` is stack safe
   * in `flatMap`.
   */
  private[data] def shift[F[_], A, B](run: A => F[B])(implicit F: FlatMap[F]): Clay[F, A, B] =
    new Clay[F, A, B] {
      def apply(a: A): F[B] = F match {
        case applic: Applicative[F] @unchecked => F.flatMap(applic.pure(a))(run)
        case _                                 => run(a)
      }
    }

  /**
   * Creates a `FunctionK` that transforms a `Clay[F, A, B]` into an `F[B]` by applying the value of type `a:A`.
   * {{{
   * scala> import cats.{~>}, cats.data.{Clay, EitherT}
   *
   * scala> def f(i: Int): Option[Either[Char, Char]] = if (i > 0) Some(Right('n')) else if (i < 0) Some(Left('z')) else None
   *
   * scala> type KOI[A] = Clay[Option, Int, A]
   * scala> val b: KOI[Either[Char, Char]] = Clay(f)
   * scala> val nt: Clay[Option, Int, ?] ~> Option = Clay.applyK[Option, Int](1)
   * scala> nt(b)
   * res0: Option[Either[Char, Char]] = Some(Right(n))
   *
   * scala> type EKOIC[A] = EitherT[KOI, Char, A]
   * scala> val c: EKOIC[Char] = EitherT[KOI, Char, Char](b)
   * scala> c.mapK(nt).value
   * res1: Option[Either[Char, Char]] = Some(Right(n))
   *
   * scala> val ntz = Clay.applyK[Option, Int](0)
   * scala> c.mapK(ntz).value
   * res2: Option[Either[Char, Char]] = None
   * }}}
   */
  def applyK[F[_], A](a: A): Clay[F, A, ?] ~> F = λ[Clay[F, A, ?] ~> F](_.apply(a))
}

sealed private[data] trait ClayFunctions {

  /**
   * Creates a Clay that ignores its input `A` and returns the given `F[B]`.
   * {{{
   * scala> import cats.data.Clay, cats.implicits._
   * scala> val takeHead = Clay[Option, List[Int], Int](_.headOption)
   * scala> val makeList = Clay.liftF[Option, Unit, List[Int]](Some(List(1,2,3)))
   * scala> (makeList andThen takeHead).apply(())
   * res0: Option[Int] = Some(1)
   * }}}
   */
  def liftF[F[_], A, B](fb: F[B]): Clay[F, A, B] = // SAM: _ => fb
    new Clay[F, A, B] { def apply(a: A): F[B] = fb }

  /**
   * Same as [[liftF]], but expressed as a FunctionK for use with mapK
   * {{{
   * scala> import cats._, data._, implicits._
   * scala> val a: OptionT[Eval, Int] = 1.pure[OptionT[Eval, ?]]
   * scala> val b: OptionT[Clay[Eval, String, ?], Int] = a.mapK(Clay.liftK)
   * scala> b.value.apply("").value
   * res0: Option[Int] = Some(1)
   * }}}
   */
  def liftK[F[_], A]: F ~> Clay[F, A, ?] = λ[F ~> Clay[F, A, ?]](Clay.liftF(_))

  /**
   * Creates a Clay arrow ignoring its input and lifting the given `B` into applicative context `F`.
   * {{{
   * scala> import cats.data.Clay, cats.implicits._
   * scala> val pureOpt = Clay.pure[Option, Unit, String]("beam me up!")
   * scala> pureOpt(())
   * res0: Option[String] = Some(beam me up!)
   * }}}
   */
  def pure[F[_], A, B](b: B)(implicit F: Applicative[F]): Clay[F, A, B] = // SAM: _ => F.pure(b)
    new Clay[F, A, B] { def apply(a: A): F[B] = F.pure(b) }

  /**
   * Creates a Clay arrow which can lift an `A` into applicative context `F`.
   * This is distinct from [[pure]] in that the input is what is lifted (and not ignored).
   * {{{
   * scala> Clay.ask[Option, Int].apply(1)
   * res0: Option[Int]: Some(1)
   * }}}
   */
  def ask[F[_], A](implicit F: Applicative[F]): Clay[F, A, A] =
    new Clay[F, A, A] { def apply(a: A): F[A] = F.pure(a) }

  /**
   * Modifies the input environment with `f`, without changing the input type of the Clay.
   * {{{
   * scala> import cats.data.Clay
   * scala> val takeHead = Clay[Option, List[Int], Int](_.headOption)
   * scala> Clay.local[Option, Int, List[Int]](1 :: _)(takeHead).apply(List(2,3))
   * res0: Option[Int] = Some(1)
   * }}}
   */
  def local[M[_], A, R](f: R => R)(clay: Clay[M, R, A]): Clay[M, R, A] =
    new Clay[M, R, A] { def apply(r: R): M[A] = clay(f(r)) }
}

sealed private[data] trait ClayFunctionsBinCompat {

  /**
   * Lifts a natural transformation of effects within a Clay
   * to a transformation of Clays.
   *
   * Equivalent to running `mapK(f) on a Clay.
   *
   * {{{
   * scala> import cats._, data._
   * scala> val f: (List ~> Option) = λ[List ~> Option](_.headOption)
   *
   * scala> val k = Clay[List, String, Char](_.toList)
   * scala> k("foo")
   * res0: List[Char] = List(f, o, o)
   *
   * scala> val k2: Clay[Option, String, Char] = Clay.liftFunctionK(f)(k)
   * scala> k2("foo")
   * res1: Option[Char] = Some(f)
   * }}}
   */
  def liftFunctionK[F[_], G[_], A](f: F ~> G): Clay[F, A, ?] ~> Clay[G, A, ?] =
    λ[Clay[F, A, ?] ~> Clay[G, A, ?]](_.mapK(f))
}

sealed private[data] trait ClayExplicitInstances {

  def endoSemigroupK[F[_]](implicit FM: FlatMap[F]): SemigroupK[λ[α => Clay[F, α, α]]] =
    Compose[Clay[F, ?, ?]].algebraK

  def endoMonoidK[F[_]](implicit M: Monad[F]): MonoidK[λ[α => Clay[F, α, α]]] =
    Category[Clay[F, ?, ?]].algebraK
}

sealed abstract private[data] class ClayInstances extends ClayInstances0 {
  implicit def catsDataMonadForClayId[A]: CommutativeMonad[Clay[Id, A, ?]] =
    catsDataCommutativeMonadForClay[Id, A]

  implicit val catsDataCommutativeArrowForClayId: CommutativeArrow[Clay[Id, ?, ?]] =
    catsDataCommutativeArrowForClay[Id]

  implicit def catsDataDeferForClay[F[_], A](implicit F: Defer[F]): Defer[Clay[F, A, ?]] =
    new Defer[Clay[F, A, ?]] {
      def defer[B](fa: => Clay[F, A, B]): Clay[F, A, B] = {
        lazy val cacheFa = fa
        new Clay[F, A, B] { def apply(a: A): F[B] = F.defer(cacheFa(a)) }
      }
    }

  implicit def catsDataFunctorFilterForClay[F[_], A](
    implicit ev: FunctorFilter[F]
  ): FunctorFilter[Clay[F, A, ?]] =
    new ClayFunctorFilter[F, A] { val FF = ev }
}

sealed abstract private[data] class ClayInstances0 extends ClayInstances0_5 {

  implicit def catsDataCommutativeArrowForClay[F[_]](
    implicit M: CommutativeMonad[F]
  ): CommutativeArrow[Clay[F, ?, ?]] with ArrowChoice[Clay[F, ?, ?]] =
    new ClayCommutativeArrow[F] { def F: CommutativeMonad[F] = M }

  implicit def catsDataCommutativeMonadForClay[F[_], A](
    implicit F0: CommutativeMonad[F]
  ): CommutativeMonad[Clay[F, A, ?]] =
    new ClayMonad[F, A] with CommutativeMonad[Clay[F, A, ?]] {
      implicit def F: Monad[F] = F0
    }

}

sealed abstract private[data] class ClayInstances0_5 extends ClayInstances1 {
  implicit def catsDataMonoidForClay[F[_], A, B](implicit FB0: Monoid[F[B]]): Monoid[Clay[F, A, B]] =
    new ClayMonoid[F, A, B] { def FB: Monoid[F[B]] = FB0 }

  implicit def catsDataMonadErrorForClay[F[_], A, E](
    implicit ME: MonadError[F, E]
  ): MonadError[Clay[F, A, ?], E] =
    new ClayMonadError[F, A, E] { def F: MonadError[F, E] = ME }

  implicit def catsDataArrowChoiceForClay[F[_]](implicit M: Monad[F]): ArrowChoice[Clay[F, ?, ?]] =
    new ClayArrowChoice[F] {
      def F: Monad[F] = M
    }

  implicit def catsDataContravariantMonoidalForClay[F[_], A](
    implicit F0: ContravariantMonoidal[F]
  ): ContravariantMonoidal[Clay[F, A, ?]] =
    new ClayContravariantMonoidal[F, A] { def F: ContravariantMonoidal[F] = F0 }

  /**
   * Witness for: Clay[M, E, A] <-> (E, R) => A
   * if M is Representable
   */
  implicit def catsDataRepresentableForClay[M[_], R, E](
    implicit
    R: Representable.Aux[M, R],
    FK: Functor[Clay[M, E, ?]]
  ): Representable.Aux[Clay[M, E, ?], (E, R)] = new Representable[Clay[M, E, ?]] {
    override type Representation = (E, R)

    override val F: Functor[Clay[M, E, ?]] = FK

    def index[A](f: Clay[M, E, A]): Representation => A = { case (e, r) => R.index(f(e))(r) }

    def tabulate[A](f: Representation => A): Clay[M, E, A] =
      new Clay[M, E, A] {
        def apply(e: E): M[A] = R.tabulate { r =>
          f(e -> r)
        }
      }
  }

}

sealed abstract private[data] class ClayInstances1 extends ClayInstances2 {
  implicit def catsDataMonadForClay[F[_], A](implicit M: Monad[F]): Monad[Clay[F, A, ?]] =
    new ClayMonad[F, A] { def F: Monad[F] = M }

  implicit def catsDataParallelForClay[F[_], M[_], A](
    implicit P: Parallel[M, F]
  ): Parallel[Clay[M, A, ?], Clay[F, A, ?]] = new Parallel[Clay[M, A, ?], Clay[F, A, ?]] {
    implicit val appF = P.applicative
    implicit val monadM = P.monad
    def applicative: Applicative[Clay[F, A, ?]] = catsDataApplicativeForClay
    def monad: Monad[Clay[M, A, ?]] = catsDataMonadForClay

    def sequential: Clay[F, A, ?] ~> Clay[M, A, ?] =
      λ[Clay[F, A, ?] ~> Clay[M, A, ?]](_.mapK(P.sequential))

    def parallel: Clay[M, A, ?] ~> Clay[F, A, ?] =
      λ[Clay[M, A, ?] ~> Clay[F, A, ?]](_.mapK(P.parallel))
  }

  implicit def catsDataContravariantForClay[F[_], C]: Contravariant[Clay[F, ?, C]] =
    new Contravariant[Clay[F, ?, C]] {
      override def contramap[A, B](fa: Clay[F, A, C])(f: B => A): Clay[F, B, C] =
        fa.local(f)
    }
}

sealed abstract private[data] class ClayInstances2 extends ClayInstances3 {
  implicit def catsDataAlternativeForClay[F[_], A](implicit F0: Alternative[F]): Alternative[Clay[F, A, ?]] =
    new ClayAlternative[F, A] { def F: Alternative[F] = F0 }
}

sealed abstract private[data] class ClayInstances3 extends ClayInstances4 {
  implicit def catsDataMonoidKForClay[F[_], A](implicit F0: MonoidK[F]): MonoidK[Clay[F, A, ?]] =
    new ClayMonoidK[F, A] { def F: MonoidK[F] = F0 }

  implicit def catsDataCommutativeFlatMapForClay[F[_], A](
    implicit F0: CommutativeFlatMap[F]
  ): CommutativeFlatMap[Clay[F, A, ?]] =
    new ClayFlatMap[F, A] with CommutativeFlatMap[Clay[F, A, ?]] { val F: CommutativeFlatMap[F] = F0 }

  implicit def catsDataChoiceForClay[F[_]](implicit M: Monad[F]): Choice[Clay[F, ?, ?]] =
    new ClayChoice[F] { def F: Monad[F] = M }

  implicit val catsDataChoiceForClayId: Choice[Clay[Id, ?, ?]] =
    catsDataChoiceForClay[Id]

  implicit def catsDataComposeForClay[F[_]](implicit FM: FlatMap[F]): Compose[Clay[F, ?, ?]] =
    new ClayCompose[F] { def F: FlatMap[F] = FM }

  implicit def catsDataStrongForClay[F[_]](implicit F0: Functor[F]): Strong[Clay[F, ?, ?]] =
    new ClayStrong[F] { def F: Functor[F] = F0 }

  implicit def catsDataSemigroupForClay[F[_], A, B](implicit FB0: Semigroup[F[B]]): Semigroup[Clay[F, A, B]] =
    new ClaySemigroup[F, A, B] { def FB: Semigroup[F[B]] = FB0 }
}

sealed abstract private[data] class ClayInstances4 extends ClayInstances5 {
  implicit def catsDataSemigroupKForClay[F[_], A](implicit F0: SemigroupK[F]): SemigroupK[Clay[F, A, ?]] =
    new ClaySemigroupK[F, A] { def F: SemigroupK[F] = F0 }

  implicit def catsDataFlatMapForClay[F[_], A](implicit FM: FlatMap[F]): FlatMap[Clay[F, A, ?]] =
    new ClayFlatMap[F, A] { def F: FlatMap[F] = FM }

}

sealed abstract private[data] class ClayInstances5 extends ClayInstances6 {

  implicit def catsDataApplicativeErrorForClay[F[_], E, A](
    implicit F0: ApplicativeError[F, E]
  ): ApplicativeError[Clay[F, A, ?], E] =
    new ClayApplicativeError[F, A, E] { def F: ApplicativeError[F, E] = F0 }
}

sealed abstract private[data] class ClayInstances6 extends ClayInstances7 {
  implicit def catsDataApplicativeForClay[F[_], A](implicit A: Applicative[F]): Applicative[Clay[F, A, ?]] =
    new ClayApplicative[F, A] { def F: Applicative[F] = A }
}

sealed abstract private[data] class ClayInstances7 extends ClayInstances8 {
  implicit def catsDataApplyForClay[F[_], A](implicit A: Apply[F]): Apply[Clay[F, A, ?]] =
    new ClayApply[F, A] { def F: Apply[F] = A }
}

sealed abstract private[data] class ClayInstances8 extends ClayInstances9 {
  implicit def catsDataDistributiveForClay[F[_], R](implicit F0: Distributive[F]): Distributive[Clay[F, R, ?]] =
    new ClayDistributive[F, R] with ClayFunctor[F, R] { implicit def F: Distributive[F] = F0 }
}

sealed abstract private[data] class ClayInstances9 {
  implicit def catsDataFunctorForClay[F[_], A](implicit F0: Functor[F]): Functor[Clay[F, A, ?]] =
    new ClayFunctor[F, A] { def F: Functor[F] = F0 }
}

private[data] trait ClayCommutativeArrow[F[_]] extends CommutativeArrow[Clay[F, ?, ?]] with ClayArrowChoice[F] {
  implicit def F: CommutativeMonad[F]
}

private[data] trait ClayArrowChoice[F[_]] extends ArrowChoice[Clay[F, ?, ?]] with ClayCategory[F] with ClayStrong[F] {
  implicit def F: Monad[F]

  def lift[A, B](f: A => B): Clay[F, A, B] =
    new Clay[F, A, B] { def apply(a: A): F[B] = F.pure(f(a)) }

  override def split[A, B, C, D](f: Clay[F, A, B], g: Clay[F, C, D]): Clay[F, (A, C), (B, D)] =
    new Clay[F, (A, C), (B, D)] {
      def apply(ac: (A, C)): F[(B, D)] = F.flatMap(f(ac._1))(b => F.map(g(ac._2))(d => (b, d)))
    }

  def choose[A, B, C, D](f: Clay[F, A, C])(g: Clay[F, B, D]): Clay[F, Either[A, B], Either[C, D]] =
    new Clay[F, Either[A, B], Either[C, D]] {
      def apply(eit: Either[A, B]): F[Either[C, D]] = eit match {
        case Left(a)  => F.map(f(a))(Left.apply _)
        case Right(b) => F.map(g(b))(Right.apply _)
      }
    }
}

private[data] trait ClayStrong[F[_]] extends Strong[Clay[F, ?, ?]] {
  implicit def F: Functor[F]

  override def lmap[A, B, C](fab: Clay[F, A, B])(f: C => A): Clay[F, C, B] =
    fab.local(f)

  override def rmap[A, B, C](fab: Clay[F, A, B])(f: B => C): Clay[F, A, C] =
    fab.map(f)

  override def dimap[A, B, C, D](fab: Clay[F, A, B])(f: C => A)(g: B => D): Clay[F, C, D] =
    fab.dimap(f)(g)

  def first[A, B, C](fa: Clay[F, A, B]): Clay[F, (A, C), (B, C)] =
    fa.first[C]

  override def second[A, B, C](fa: Clay[F, A, B]): Clay[F, (C, A), (C, B)] =
    fa.second[C]
}

private[data] trait ClayChoice[F[_]] extends Choice[Clay[F, ?, ?]] with ClayCategory[F] {
  def choice[A, B, C](f: Clay[F, A, C], g: Clay[F, B, C]): Clay[F, Either[A, B], C] =
    new Clay[F, Either[A, B], C] {
      def apply(eab: Either[A, B]): F[C] = eab.fold(f.apply, g.apply)
    }
}

private[data] trait ClayCategory[F[_]] extends Category[Clay[F, ?, ?]] with ClayCompose[F] {
  implicit def F: Monad[F]

  override def id[A]: Clay[F, A, A] = Clay.ask[F, A]
}

private[data] trait ClayCompose[F[_]] extends Compose[Clay[F, ?, ?]] {
  implicit def F: FlatMap[F]

  def compose[A, B, C](f: Clay[F, B, C], g: Clay[F, A, B]): Clay[F, A, C] =
    f.compose(g)
}

private[data] trait ClaySemigroup[F[_], A, B] extends Semigroup[Clay[F, A, B]] {
  implicit def FB: Semigroup[F[B]]

  override def combine(x: Clay[F, A, B], y: Clay[F, A, B]): Clay[F, A, B] =
    new Clay[F, A, B] { def apply(a: A): F[B] = FB.combine(x(a), y(a)) }
}

private[data] trait ClayMonoid[F[_], A, B] extends Monoid[Clay[F, A, B]] with ClaySemigroup[F, A, B] {
  implicit def FB: Monoid[F[B]]

  override def empty: Clay[F, A, B] = new Clay[F, A, B] { def apply(a: A): F[B] = FB.empty }
}

sealed private[data] trait ClaySemigroupK[F[_], A] extends SemigroupK[Clay[F, A, ?]] {
  implicit def F: SemigroupK[F]

  override def combineK[B](x: Clay[F, A, B], y: Clay[F, A, B]): Clay[F, A, B] =
    new Clay[F, A, B] { def apply(a: A): F[B] = F.combineK(x(a), y(a)) }
}

sealed private[data] trait ClayMonoidK[F[_], A] extends MonoidK[Clay[F, A, ?]] with ClaySemigroupK[F, A] {
  implicit def F: MonoidK[F]

  override def empty[B]: Clay[F, A, B] = Clay.liftF(F.empty[B])
}

private[data] trait ClayAlternative[F[_], A]
    extends Alternative[Clay[F, A, ?]]
    with ClayApplicative[F, A]
    with ClayMonoidK[F, A] {
  implicit def F: Alternative[F]
}

sealed private[data] trait ClayContravariantMonoidal[F[_], D] extends ContravariantMonoidal[Clay[F, D, ?]] {
  implicit def F: ContravariantMonoidal[F]

  override def unit: Clay[F, D, Unit] = new Clay[F, D, Unit] { def apply(d: D): F[Unit] = F.unit }

  override def contramap[A, B](fa: Clay[F, D, A])(f: B => A): Clay[F, D, B] =
    new Clay[F, D, B] { def apply(d: D): F[B] = F.contramap(fa(d))(f) }

  override def product[A, B](fa: Clay[F, D, A], fb: Clay[F, D, B]): Clay[F, D, (A, B)] =
    new Clay[F, D, (A, B)] { def apply(d: D): F[(A, B)] = F.product(fa(d), fb(d)) }
}

private[data] trait ClayMonadError[F[_], A, E]
    extends MonadError[Clay[F, A, ?], E]
    with ClayApplicativeError[F, A, E]
    with ClayMonad[F, A] {
  def F: MonadError[F, E]
}

private[data] trait ClayApplicativeError[F[_], A, E]
    extends ApplicativeError[Clay[F, A, ?], E]
    with ClayApplicative[F, A] {
  type K[T] = Clay[F, A, T]

  implicit def F: ApplicativeError[F, E]

  def raiseError[B](e: E): K[B] =
    new Clay[F, A, B] { def apply(a: A): F[B] = F.raiseError(e) }

  def handleErrorWith[B](kb: K[B])(f: E => K[B]): K[B] =
    new Clay[F, A, B] { def apply(a: A): F[B] = F.handleErrorWith(kb(a))((e: E) => f(e)(a)) }

}

private[data] trait ClayMonad[F[_], A] extends Monad[Clay[F, A, ?]] with ClayFlatMap[F, A] with ClayApplicative[F, A] {
  implicit def F: Monad[F]
}

private[data] trait ClayFlatMap[F[_], A] extends FlatMap[Clay[F, A, ?]] with ClayApply[F, A] {
  implicit def F: FlatMap[F]

  def flatMap[B, C](fa: Clay[F, A, B])(f: B => Clay[F, A, C]): Clay[F, A, C] = fa.flatMap(f)

  def tailRecM[B, C](b: B)(f: B => Clay[F, A, Either[B, C]]): Clay[F, A, C] =
    new Clay[F, A, C] { def apply(a: A): F[C] = F.tailRecM(b)(f(_)(a)) }
}

private[data] trait ClayApplicative[F[_], A] extends Applicative[Clay[F, A, ?]] with ClayApply[F, A] {
  implicit def F: Applicative[F]

  def pure[B](b: B): Clay[F, A, B] = Clay.pure(b)
}

private[data] trait ClayApply[F[_], A] extends Apply[Clay[F, A, ?]] with ClayFunctor[F, A] {
  implicit def F: Apply[F]

  override def ap[B, C](f: Clay[F, A, B => C])(fa: Clay[F, A, B]): Clay[F, A, C] = fa.ap(f)

  override def product[B, C](fb: Clay[F, A, B], fc: Clay[F, A, C]): Clay[F, A, (B, C)] = fb.merge(fc)
}

private[data] trait ClayFunctor[F[_], A] extends Functor[Clay[F, A, ?]] {
  implicit def F: Functor[F]

  override def map[B, C](fa: Clay[F, A, B])(f: B => C): Clay[F, A, C] = fa.map(f)
}

private trait ClayDistributive[F[_], R] extends Distributive[Clay[F, R, ?]] {
  implicit def F: Distributive[F]

  override def distribute[G[_]: Functor, A, B](a: G[A])(f: A => Clay[F, R, B]): Clay[F, R, G[B]] =
    new Clay[F, R, G[B]] { def apply(r: R): F[G[B]] = F.distribute(a)(f(_).apply(r)) }

  def map[A, B](fa: Clay[F, R, A])(f: A => B): Clay[F, R, B] = fa.map(f)
}

private[this] trait ClayFunctorFilter[F[_], R] extends FunctorFilter[Clay[F, R, ?]] {

  def FF: FunctorFilter[F]

  def functor: Functor[Clay[F, R, ?]] = Clay.catsDataFunctorForClay(FF.functor)

  def mapFilter[A, B](fa: Clay[F, R, A])(f: A => Option[B]): Clay[F, R, B] =
    new Clay[F, R, B] { def apply(r: R): F[B] = FF.mapFilter(fa(r))(f) }
}
