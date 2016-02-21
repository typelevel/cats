package cats
package arrow

/**
 *
 * Note: this is NOT library code
 *
 * it is an example illustrating library code
 *
 * should eventually be removed
 *
 */

object ArrowApp {

  class ArrowModule[F[_, _]: Arrow] {

    // a few local definitions

    def lift[Z, Y](z2y: Z => Y): F[Z, Y] = implicitly[Arrow[F]].lift[Z, Y](z2y)

    def define[Z, Y, X](z2y: Z => Y): F[(Z, Y), X] => F[Z, X] = implicitly[Arrow[F]].define[Z, Y, X](z2y)

    def value[Z, Y, X](y: Y): F[(Z, Y), X] => F[Z, X] = implicitly[Arrow[F]].value[Z, Y, X](y)

    def `def`[Z, Y, X](z2y: Z => Y): F[Y, Z => X] => F[Z, X] = implicitly[Arrow[F]].`def`[Z, Y, X](z2y)

    def `val`[Z, Y, X](y: Y): F[Y, Z => X] => F[Z, X] = implicitly[Arrow[F]].`val`[Z, Y, X](y)

    /**
     * for this simple example
     *
     *  we do not use the defined variables that are in scope (we could, using variable names instead of `_`)
     *  we also do not do any side effects (we could, using `flatDefine` instead of `define`)
     */

    val helloCats01: F[Unit, String] =
      define { _: Unit => "Hello" } {
        define { _: (Unit, String) => ',' } {
          define { _: ((Unit, String), Char) => " Cats" } {
            define { _: (((Unit, String), Char), String) => '!' } {
              lift {
                case ((((_, hello), comma), cats), bang) =>
                  s"$hello$comma$cats$bang"
              }
            }
          }
        }
      }

    /**
     * since we do not use the defined variables that are in scope, we might as well use `value` instead of `define`
     */

    val helloCats02: F[Unit, String] =
      value ("Hello") {
        value (',') {
          value (" Cats") {
            value ('!') {
              lift {
                case ((((_, hello), comma), cats), bang) =>
                  s"$hello$comma$cats$bang"
              }
            }
          }
        }
      }

    /**
     * for this simple example
     *
     *  we do not use the defined variables that are in scope (we could, using variable names instead of `_`)
     *  we also do not do any side effects (we could, using `flatDefine` instead of `define`)
     */

    val helloCats03: F[Unit, String] =
      `def` { _: Unit => "Hello" } {
        `def` { _: String => ',' } {
          `def` { _: Char => " Cats" } {
            `def` { _: String => '!' } {
              lift {
                bang =>
                  cats =>
                    comma =>
                      hello =>
                        _ =>
                          s"$hello$comma$cats$bang"
              }
            }
          }
        }
      }

    /**
     * since we do not use the defined variables that are in scope, we might as well use ``val`` instead of ``def``
     */

    val helloCats04: F[Unit, String] =
      `val` ("Hello") {
        `val` (',') {
          `val` (" Cats") {
            `val` ('!') {
              lift {
                bang =>
                  cats =>
                    comma =>
                      hello =>
                        _ =>
                          s"$hello$comma$cats$bang"
              }
            }
          }
        }
      }

  }

  def main(args: Array[String]): Unit = {
    implicit val function1Arrow: Arrow[Function1] =
      new Arrow[Function1] {

        def lift[A, B](f: A => B): A => B = f

        override def first[A, B, C](fa: A => B): ((A, C)) => (B, C) = {
          case (a, c) => (fa(a), c)
        }

        def id[A]: A => A = a => a

        override def split[A, B, C, D](f: A => B, g: C => D): ((A, C)) => (B, D) = {
          case (a, c) => (f(a), g(c))
        }

        def compose[A, B, C](f: B => C, g: A => B): A => C = f.compose(g)
      }

    object arrowObject extends ArrowModule[Function1]

    import arrowObject._

    println(helloCats01(()))
    println(helloCats02(()))
    println(helloCats03(()))
    println(helloCats04(()))

  }

}
