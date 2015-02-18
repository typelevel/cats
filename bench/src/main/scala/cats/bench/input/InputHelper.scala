package cats.bench.input

import scala.reflect.ClassTag
import scala.util.Random

trait InputHelper {

  val r: Random = new Random()

  def genArray[A: ClassTag](size:Int)(f: => A): Array[A] = {
    val data = Array.ofDim[A](size)
    for (i <- 0 until size) data(i) = f
    data
  }

}
