package cats
package syntax

trait ShowSyntax extends Show.ToShowOps {
  implicit final def showInterpolator(sc: StringContext): Show.ShowInterpolator = Show.ShowInterpolator(sc)
}
