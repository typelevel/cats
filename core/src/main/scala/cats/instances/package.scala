package cats.instances

import scala.collection.mutable.Builder

private[instances] object instances {

  def appendAll[F <: Iterable[A], A](it: Iterator[F], bldr: Builder[A, F]): bldr.type = {
    while (it.hasNext) {
      bldr ++= it.next()
    }
    bldr
  }

}
