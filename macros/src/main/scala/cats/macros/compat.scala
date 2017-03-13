package cats
package macros

/** Macro compatibility.
  *
  * Used only to push deprecation errors in core off into
  * the macros project, as warnings.
  */
private[cats] class MacroCompat {

  type Context = reflect.macros.Context
  def compatNewTypeName(c: Context, name: String): c.TypeName =
    c.universe.newTypeName(name)

}
