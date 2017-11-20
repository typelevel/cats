package fix
package to1_0_0

object RenameTupleApplySyntaxTests {
  import cats.{Eq, Semigroup}
  import cats.instances.all._
  import cats.syntax.apply._

  (Option(1), Option(2)).mapN(_ + _)
  (Option(1), Option(2), Option(3)).mapN(_ + _ + _)
  (Option(1), Option(2), Option(3), Option(4)).mapN(_ + _ + _ + _)

  case class Foo2(a: Int, b: Int)
  case class Foo3(a: Int, b: Int, c: Int)
  case class Foo4(a: Int, b: Int, c: Int, d: Int)

  (Eq[Int], Eq[Int]).contramapN((f: Foo2) => (f.a, f.b))
  (Eq[Int], Eq[Int], Eq[Int]).contramapN((f: Foo3) => (f.a, f.b, f.c))
  (Eq[Int], Eq[Int], Eq[Int], Eq[Int]).contramapN((f: Foo4) =>
    (f.a, f.b, f.c, f.d))

  (Semigroup[Int], Semigroup[Int])
    .imapN(Foo2.apply)(Function.unlift(Foo2.unapply))
  (Semigroup[Int], Semigroup[Int], Semigroup[Int])
    .imapN(Foo3.apply)(Function.unlift(Foo3.unapply))
  (Semigroup[Int], Semigroup[Int], Semigroup[Int], Semigroup[Int])
    .imapN(Foo4.apply)(Function.unlift(Foo4.unapply))
}
