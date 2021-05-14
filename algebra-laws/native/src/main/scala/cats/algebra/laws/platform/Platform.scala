package cats.algebra.laws.platform

private[laws] object Platform {
  // using `final val` makes compiler constant-fold any use of these values, dropping dead code automatically
  // $COVERAGE-OFF$
  final val isJvm = false
  final val isJs = false
  final val isNative = true
  // $COVERAGE-ON$
}
