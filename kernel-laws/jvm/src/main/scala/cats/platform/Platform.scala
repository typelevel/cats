package cats.platform

private[cats] object Platform {
  // using `final val` makes compiler constant-fold any use of these values, dropping dead code automatically
  // $COVERAGE-OFF$
  final val isJvm = true
  final val isJs = false
  final val isNative = false
  // $COVERAGE-ON$
}
