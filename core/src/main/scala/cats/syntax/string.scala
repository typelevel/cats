package cats.syntax

trait StringSyntax {
  implicit final def catsSyntaxString(s: String): StringOps = new StringOps(s)
}

final class StringOps(private val s: String) extends AnyVal {

  /**
   * Wraps a `String` in `Throwable`.
   *
   * {{{
   *  scala> import cats.syntax.string._
   *
   *  scala> "new throwable".asThrowable
   *  res0: Throwable = java.lang.Throwable: new throwable
   * }}}
   */
  def asThrowable: Throwable = new Throwable(s)

  /**
   * Wraps a `String` in `Error`.
   *
   * {{{
   *  scala> import cats.syntax.string._
   *
   *  scala> "new error".asError
   *  res0: Error = java.lang.Error: new error
   * }}}
   */
  def asError: Error = new Error(s)

  /**
   * Wraps a `String` in `Exception`.
   *
   * TODO:  doctest v0.9.9 is having import conflicts between `java.lang.Exception`
   *        and `org.scalacheck.Prop.Exception`, add example when fixed
   */
  def asException: Exception = new Exception(s)
}
