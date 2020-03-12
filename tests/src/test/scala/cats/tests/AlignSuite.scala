package cats.tests

import cats.Align
import cats.instances.all._
import cats.kernel.laws.discipline.SemigroupTests

class AlignSuite extends CatsSuite {
  {
    val optionSemigroup = Align.semigroup[Option, Int]
    checkAll("Align[Option].semigroup", SemigroupTests[Option[Int]](optionSemigroup).semigroup)

    val listSemigroup = Align.semigroup[List, String]
    checkAll("Align[List].semigroup", SemigroupTests[List[String]](listSemigroup).semigroup)
  }
}
