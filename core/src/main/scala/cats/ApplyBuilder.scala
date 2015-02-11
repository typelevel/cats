package cats

trait ApplyBuilder[F[_], A] {
  val a: F[A]
  @macros.apply.builders trait ApplyBuilder2

  def |@|[B](b0: F[B]): ApplyBuilder2[B] = new ApplyBuilder2[B] {
    val b: F[B] = b0
  }

}

