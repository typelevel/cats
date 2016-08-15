package cats
package tests

import cats.data.{EitherT,OptionT,XorT,WriterT,Kleisli, StateT}

class TransLiftTests extends CatsSuite {

  case class NoTypeclass[A](a: A)

  case class JustFunctor[A](a: A)

  implicit val jfFunctor: Functor[JustFunctor] = new Functor[JustFunctor] {
    override def map[A, B](fa: JustFunctor[A])(f: A => B): JustFunctor[B] = JustFunctor(f(fa.a))
  }

  case class JustAp[A](a: A)

  implicit val jfApp: Applicative[JustAp] = new Applicative[JustAp] {
    override def pure[A](a: A): JustAp[A] = JustAp(a)
    override def ap[A, B](ff: JustAp[A => B])(fa: JustAp[A]): JustAp[B] = JustAp(ff.a(fa.a))
    override def product[A, B](fa: JustAp[A],fb: JustAp[B]): JustAp[(A, B)] = JustAp(fa.a -> fb.a)
    override def map[A, B](fa: JustAp[A])(f: A => B): JustAp[B] = JustAp(f(fa.a))
  }

  test("transLift for EitherT, XorT, OptionT, WriterT requires only Functor") {
    val e: EitherT[JustFunctor, Int, Int]    = JustFunctor(1).liftT[λ[(α[_], β) => EitherT[α, Int, β]]]
    val d: XorT[JustFunctor, Int, Int]    = JustFunctor(1).liftT[λ[(α[_], β) => XorT[α, Int, β]]]
    val c: OptionT[JustFunctor, Int]      = JustFunctor(1).liftT[OptionT]
    val a: WriterT[JustFunctor, Int, Int] = JustFunctor(1).liftT[λ[(α[_], β) => WriterT[α, Int, β]]]
  }

  test("transLift for StateT requires Applicative Functor") {
    val f: StateT[JustAp, Int, Int] = JustAp(1).liftT[λ[(α[_], β) => StateT[α, Int, β]]]
  }

  test("transLift for Kleisli doesn't require anything of the wrapped value"){
    val e: Kleisli[NoTypeclass, Int, Int] = NoTypeclass(1).liftT[λ[(α[_], β) => Kleisli[α, Int, β]]]
  }
}
