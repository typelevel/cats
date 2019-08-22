package cats.kernel
package instances

trait HashInstances extends HashToHashingConversion

object hash extends HashInstances with HashToHashingConversionBinCompat0
