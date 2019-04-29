package cats.data
import cats.implicits._

object CatsDataPackagePrivateMimaExceptions {
  def isBinaryCompatible = (
    cats.data.NonEmptySetImpl.catsDataEqForNonEmptySet[Int]
  )
}
