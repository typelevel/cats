import sbt._

object FreeBoiler extends BoilerPlateHelper {
  val templates: Seq[Template] = Seq(FreeSyntax)

  val header = "// auto-generated boilerplate by /project/FreeBoiler.scala"

  object FreeSyntax extends DefaultTemplate {
    override def filename(root: sbt.File) = root / "cats" / "free" / "FreeSyntax.scala"

    override def content(tv: TemplateVals): String = {
      import tv._

      block"""
      |package cats
      |package free
      |
      |trait FreeSyntax {
      |  implicit class WithFreeSyntax0[F[_], A](fa: F[A]) {
      |    def free: Free[F, A] = Free.liftF[F, A](fa)
      |  }
      |
      -  implicit class WithFreeSyntax$arity[${`A..N`}, F[_], A](fa: (${`A..N`}) => F[A]) {
      -    def free: (${`A..N`}) => Free[F, A] = (${`a:A..n:N`}) => Free.liftF[F, A](fa(${`a..n`}))
      -  }
      |}
      """
    }
  }
}
