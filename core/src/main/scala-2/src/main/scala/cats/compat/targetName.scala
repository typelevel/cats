package cats.compat

import scala.annotation.Annotation

// compat dummy so we can use targetName on scala 3 to get out of bincompat pickles
private[cats] class targetName(dummy: String) extends Annotation
