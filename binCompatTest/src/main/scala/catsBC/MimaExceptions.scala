package catsBC
import cats.implicits._
import cats._, data._, cats.arrow._

object MimaExceptions {
  import cats.implicits._

  def isBinaryCompatible = (
      Monad[OptionT[List, ?]],
      cats.data.OptionT.catsDataTraverseForOptionT[List],
      cats.data.Kleisli.catsDataCommutativeArrowForKleisliId,
      cats.data.OptionT.catsDataMonoidKForOptionT[List],
      cats.data.OptionT.catsDataMonoidForOptionT[List, Int],
      cats.data.Kleisli.catsDataMonadForKleisliId[Int],
      cats.data.Kleisli.catsDataCommutativeArrowForKleisli[Option],
      cats.data.Kleisli.catsDataCommutativeFlatMapForKleisli[Option, Int],
      cats.data.IRWST.catsDataStrongForIRWST[List, Int, Int, Int],
      cats.data.OptionT.catsDataMonadErrorMonadForOptionT[List]
  )

}
