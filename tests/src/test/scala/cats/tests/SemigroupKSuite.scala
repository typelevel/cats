package cats.tests

import cats.SemigroupK
import cats.data.{Chain, Validated}
import cats.laws.discipline.AlignTests
import cats.laws.discipline.arbitrary._

class SemigroupKSuite extends CatsSuite {
  {
    implicit val listwrapperSemigroupK = ListWrapper.alternative
    implicit val listwrapperAlign = SemigroupK.align[ListWrapper]
    checkAll("SemigroupK[ListWrapper].align", AlignTests[ListWrapper].align[Int, Int, Int, Int])

    implicit val validatedAlign = SemigroupK.align[Validated[String, *]]
    checkAll("SemigroupK[Validated].align", AlignTests[Validated[String, *]].align[Int, Int, Int, Int])

    implicit val chainAlign = SemigroupK.align[Chain]
    checkAll("SemigroupK[Chain].align", AlignTests[Chain].align[Int, Int, Int, Int])
  }
}
