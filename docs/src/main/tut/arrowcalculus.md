---
layout: default
title:  "Arrow Calculus"
section: "typeclasses"
source: "https://github.com/non/cats/blob/master/core/src/main/scala/cats/Monad.scala"
scaladoc: "#cats.Arrow"
-----------------------
# Arrow Calculus

The `Arrow` trait has methods `flatDefine`, `flatDef`, `define`, ``def`` that define a *DSL* similar to the `Monad` and `Applicative` DSL's.

* `flatDefine` is the uncurried version of `flatDef`
* `flatDef` is the curried version of `flatDefine`

* `define` is the uncurried version of ``def``
* ``def`` is the curried version of `define`

The methods above are related to the functionality described in [The Arrow Calculus](http://homepages.inf.ed.ac.uk/wadler/papers/arrows/arrows.pdf).
In short: *arrow calculus* can be seen as *impure lambda calculus* (*lambda calculus with side effects*).

The arrow methods `define`, ``def`` (and simplified versions `value` and ``val``) correspond to the *pure* part of the calculus.
The arrow methods `flatDefine` and `flatDef` correspond to the *impure* part of the calculus.

Compare this with the difference between the pure resp. impure monad methods `map` resp. `flatMap`.


The examples below only make use of  `define`, ``def``, `value` and ``val``.
They do not deal with side effects, but, of course, side effects can be dealt with as well.

```tut
import cats.arrow._

object ArrowApp {

  class ArrowModule[F[_, _]: Arrow] {

    // a few local definitions

    def lift[Z, Y](z2y: Z => Y): F[Z, Y] = implicitly[Arrow[F]].lift[Z, Y](z2y)

    def define[Z, Y, X](z2y: Z => Y): F[(Z, Y), X] => F[Z, X] = implicitly[Arrow[F]].define[Z, Y, X](z2y)

    def value[Z, Y, X](y: Y): F[(Z, Y), X] => F[Z, X] = implicitly[Arrow[F]].value[Z, Y, X](y)

    def `def`[Z, Y, X](z2y: Z => Y): F[Y, Z => X] => F[Z, X] = implicitly[Arrow[F]].`def`[Z, Y, X](z2y)

    def `val`[Z, Y, X](y: Y): F[Y, Z => X] => F[Z, X] = implicitly[Arrow[F]].`val`[Z, Y, X](y)

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

```