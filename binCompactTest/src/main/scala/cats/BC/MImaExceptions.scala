package cats
package BC

object MimaExceptions {
  import cats.implicits._
  import cats._, data._
  def isBinaryCompatible = {

    Monad[OptionT[List, ?]].pure(1)
    var a: Any = cats.data.OptionT.catsDataTraverseForOptionT[List]
    a = cats.data.Kleisli.catsDataCommutativeArrowForKleisliId
    a = cats.data.OptionT.catsDataMonoidKForOptionT[List]
    a = cats.data.OptionT.catsDataMonoidForOptionT[List, Int]
    a = cats.data.Kleisli.catsDataMonadForKleisliId[Int]
    a = cats.data.Kleisli.catsDataCommutativeArrowForKleisli[Option]
    a = cats.data.Kleisli.catsDataCommutativeFlatMapForKleisli[Option, Int]
    a = cats.data.IRWST.catsDataStrongForIRWST[List, Int, Int, Int]
    a = cats.data.OptionT.catsDataMonadErrorMonadForOptionT[List]

    a = "e".leftNec[Int]


    true

  }
}
