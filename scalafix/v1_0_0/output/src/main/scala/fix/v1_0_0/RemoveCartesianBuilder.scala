package fix
package to1_0_0

object RemoveCartesianBuilderTests {
  {
    import cats.instances.option._
    import cats.instances.int._
    import cats.syntax.apply._
    import cats.syntax.semigroup._
    val o1: Option[Int] = Some(42)
    val o2: Option[String] = Some("hello")
    val o3: Option[Int] = Some(2)
    (o1, o2).mapN((i: Int, s: String) => i.toString ++ s)
    (o1, o2).tupled
    (o1, o2, o3).mapN(_ + _ + _)
    (o1 |+| o1, o3).mapN(_ + _)
    (o1, o2, o3) mapN (_ + _ + _)
    (o1 |+| o1, o3) mapN (_ + _)
    (o1 |+| o1, o3) mapN (_ + _)

    (o1,
     o2,
     o3) mapN (_ + _ + _)
  }

  {
    import cats.{Semigroup, Eq}
    import cats.implicits._
    case class Foo(a: String, c: List[Double])

    (Semigroup[String], Semigroup[List[Double]])
      .imapN(Foo.apply)(Function.unlift(Foo.unapply))

    (Eq[Double], Eq[String]).contramapN { (a: Foo) =>
      (2, "bar")
    }
  }
}
