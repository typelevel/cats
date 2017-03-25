package cats
package tests

class AsTests extends CatsSuite {
  import evidence._

  def toMap[A, B, X](fa: List[X])(implicit ev: X <~< (A,B)): Map[A,B] =
    fa.foldLeft(Map.empty[A,B])(As.contra2_3(ev)(_ + _))

  test("narrow an input of a function2") {
    // scala's GenTraversableOnce#toMap has a similar <:< constraint

    toMap(List("String" -> 1))
  }

  test("lift <:") {
    // scala's GenTraversableOnce#toMap has a similar <:< constraint
    trait A
    case class Foo(x: Int) extends A

    val lifted: Foo <~< A = As.unsafeFromPredef[Foo,A]
    toMap(List("String" -> Foo(1)))(As.co2_2(lifted))
  }

  test("check expected relationships") {
    // scala's GenTraversableOnce#toMap has a similar <:< constraint
    implicitly[Int <~< Any]
    implicitly[String <~< Any]
    implicitly[String <~< AnyRef]
    implicitly[String <~< AnyRef]
    implicitly[(String,Int) <~< (AnyRef,Any)]
    implicitly[scala.collection.immutable.List[String] <~< scala.collection.Seq[Any]]
  }
}
