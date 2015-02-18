lazy val maxArity = 12

def genApplyBuilders(dir: File): Seq[File] = {
  def builder(N: Int): String = {
    val typeArgs = (1 to N).map(i => s"A$i").mkString(", ")
    val termArgs = (1 to N).map(i => s"a$i").mkString(", ")

    val next = if (N < maxArity) s"""
      def |@|[Z](z: F[Z]): ApplyBuilder${N + 1}[Z] = new ApplyBuilder${N + 1}[Z](z)

      ${builder(N + 1)}
    """ else ""

    s"""
      private[syntax] class ApplyBuilder$N[A$N](a$N: F[A$N]) {
        def apply[Z](f: F[($typeArgs) => Z])(implicit F: Apply[F]): F[Z] = F.apply$N($termArgs)(f)
        def map[Z](f: ($typeArgs) => Z)(implicit F: Apply[F]): F[Z] = F.map$N($termArgs)(f)
        def tupled[Z](implicit F: Apply[F]): F[($typeArgs)] = F.tuple$N($termArgs)
        $next
      }
    """
  }

  val source = s"""
    package cats
    package syntax

    private[syntax] class ApplyBuilder[F[_], A1](a1: F[A1]) {
      def |@|[A2](a2: F[A2]): ApplyBuilder2[A2] = new ApplyBuilder2[A2](a2)

      ${builder(2)}
    }
  """

  val file = dir / "cats" / "syntax" / "ApplyBuilder.scala"

  IO.write(file, source)

  Seq(file)
}

sourceGenerators in Compile <+= (sourceManaged in Compile).map(genApplyBuilders)
