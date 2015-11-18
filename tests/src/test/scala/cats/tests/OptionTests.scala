package cats
package tests

import cats.laws.discipline.{TraverseTests, CoflatMapTests, MonadCombineTests, SerializableTests, MonoidalTests}
import cats.laws.discipline.eq._

class OptionTests extends CatsSuite {
  checkAll("Option[Int]", MonoidalTests[Option].monoidal[Int, Int, Int])
  checkAll("Monoidal[Option]", SerializableTests.serializable(Monoidal[Option]))

  checkAll("Option[Int]", CoflatMapTests[Option].coflatMap[Int, Int, Int])
  checkAll("CoflatMap[Option]", SerializableTests.serializable(CoflatMap[Option]))

  checkAll("Option[Int]", MonadCombineTests[Option].monadCombine[Int, Int, Int])
  checkAll("MonadCombine[Option]", SerializableTests.serializable(MonadCombine[Option]))

  checkAll("Option[Int] with Option", TraverseTests[Option].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("Traverse[Option]", SerializableTests.serializable(Traverse[Option]))
}
