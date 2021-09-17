package algebra.laws

/**
 * This object contains Arbitrary instances for types defined in
 * algebra.instances, as well as anything else we'd like to import to assist
 * in running ScalaCheck tests.
 *
 * (Since algebra-instances has no dependencies, its types can't
 * define Arbitrary instances in companions.)
 */
@deprecated("No replacement", since = "2.7.0")
object CheckSupport {}
