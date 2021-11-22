/*
rule = "scala:fix.v1_0_0.SimplifyEitherTLift"
 */
package fix
package to1_0_0

object SimplifyEitherTLiftTests {
  import cats.Id
  import cats.data.EitherT
  val eithert = EitherT.left[Id, String, Int]("eithert")
  eithert.recoverWith {
    case "eithert" => EitherT.right[Id, String, Int](5)
  }.isRight
}
