package cats.platform

private[cats] object Platform {
  // using `final val` makes compiler constant-fold any use of these values, dropping dead code automatically
  // $COVERAGE-OFF$
  final val isJvm = false
  final val isJs = true
  // $COVERAGE-ON$
}
