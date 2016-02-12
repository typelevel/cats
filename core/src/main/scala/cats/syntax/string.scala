package cats
package syntax

import cats.data.Xor

trait StringSyntax {
  implicit def stringSyntax(s: String): StringOps = new StringOps(s)
}

final class StringOps(val s: String) extends AnyVal {

  def parseBoolean: IllegalArgumentException Xor Boolean = Xor.catchOnly[IllegalArgumentException](s.toBoolean)

  def parseBooleanOption: Option[Boolean] = parseBoolean.toOption

  def parseByte: NumberFormatException Xor Byte = parse[Byte](_.toByte)

  def parseByteOption: Option[Byte] = parseByte.toOption

  def parseDouble: NumberFormatException Xor Double = parse[Double](_.toDouble)

  def parseDoubleOption: Option[Double] = parseDouble.toOption

  def parseFloat: NumberFormatException Xor Float = parse[Float](_.toFloat)

  def parseFloatOption: Option[Float] = parseFloat.toOption

  def parseInt: NumberFormatException Xor Int = parse[Int](_.toInt)

  def parseIntOption: Option[Int] = parseInt.toOption

  def parseLong: NumberFormatException Xor Long = parse[Long](_.toLong)

  def parseLongOption: Option[Long] = parseLong.toOption

  def parseShort: NumberFormatException Xor Short = parse[Short](_.toShort)

  def parseShortOption: Option[Short] = parseShort.toOption

  private def parse[A](f: String => A): NumberFormatException Xor A = Xor.catchOnly[NumberFormatException](f(s))

}
