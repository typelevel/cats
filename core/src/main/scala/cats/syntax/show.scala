package cats
package syntax

trait ShowSyntax extends Show.ToShowOps {
  implicit def showInterpolator(sc: StringContext): Show.ShowInterpolator = Show.ShowInterpolator(sc)
}
