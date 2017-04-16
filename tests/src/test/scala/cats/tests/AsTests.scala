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
    trait Bar
    case class Foo(x: Int) extends Bar

    val lifted: Foo <~< Bar = As.reify[Foo, Bar]
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

  trait Top {
    def foo: String = this.getClass.getName
  }
  trait Middle extends Top
  case class Bottom() extends Middle

  test("subtyping relationships compose") {

    val cAsB: Bottom As Middle = As.reify[Bottom,Middle]
    val bAsA: Middle As Top = As.fromPredef(implicitly)

    val one: Bottom As Top = cAsB andThen bAsA
    val two: Bottom As Top = bAsA compose cAsB 
  }


  test("we can use As to coerce a value") {
    val cAsA: Bottom As Top = implicitly

    val c: Bottom = Bottom()

    val a: Top = cAsA.coerce(c)
    a.foo
  }

  test("we can lift subtyping to covariant type constructors") {
    val cAsA: Bottom As Top = implicitly
    val co: List[Bottom] As List[Top] = As.co(cAsA)
    val co2: ((Bottom, String) As (Top, String)) = As.co2(cAsA)
    val co2_2: ((String, Bottom) As (String, Top)) = As.co2_2(cAsA)
    val co3: ((Bottom, Unit, Unit) As (Top, Unit, Unit)) = As.co3(cAsA)
    val co3_2: ((Unit, Bottom, Unit) As (Unit, Top, Unit)) = As.co3_2(cAsA)
    val co3_3: ((Unit, Unit, Bottom) As (Unit, Unit, Top)) = As.co3_3(cAsA)
    val lift2: ((Bottom, String) As (Top,Any)) = As.lift2(cAsA,implicitly)
  }

  test("we can widen a function1") {
    val f: Any => Bottom = _ => Bottom()
    val cAsA: Bottom As Top = implicitly
    val f2: Any => Top = As.onF(cAsA)(f)
  }
}
