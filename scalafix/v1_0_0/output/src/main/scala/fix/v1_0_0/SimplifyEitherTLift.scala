package fix
package to1_0_0

object SimplifyEitherTLiftTests {
  import cats.Id
  import cats.data.EitherT
  val eithert = EitherT.leftT[Id, Int]("eithert")
  eithert.recoverWith {
    case "eithert" => EitherT.pure[Id, String](5)
  }.isRight
}
