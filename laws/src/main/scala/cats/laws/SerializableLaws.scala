package cats
package laws

import org.scalacheck.Prop
import org.scalacheck.Prop.{ Exception, Proof, Result }

import catalysts.Platform

import scala.util.control.NonFatal

/**
 * Check for Java Serializability.
 *
 * This law is only applicable on the JVM, but is something we want
 * to be sure to enforce. Therefore, we use bricks.Platform to do a
 * runtime check rather than create a separate jvm-laws project.
 */
object SerializableLaws {

  // This part is a bit tricky. Basically, we only want to test
  // serializability on the JVM.
  //
  // The Platform.isJs macro will give us a literal true or false at
  // compile time, so we rely on scalac to prune away the "other"
  // branch. Thus, when scala.js look at this method it won't "see"
  // the branch which was removed, and will avoid an error trying to
  // suport java.io.*.
  //
  // This ends up being a lot nicer than having to split the entire
  // laws project.

  def serializable[A](a: A): Prop =
    if (Platform.isJs) Prop(_ => Result(status = Proof)) else Prop { _ =>
      import java.io.{ ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream }

      val baos = new ByteArrayOutputStream()
      val oos = new ObjectOutputStream(baos)
      var ois: ObjectInputStream = null // scalastyle:ignore null
      try {
        oos.writeObject(a)
        oos.close()
        val bais = new ByteArrayInputStream(baos.toByteArray())
        ois = new ObjectInputStream(bais)
        val a2 = ois.readObject()
        ois.close()
        Result(status = Proof)
      } catch { case NonFatal(t) =>
        Result(status = Exception(t))
      } finally {
        oos.close()
        if (ois != null) ois.close() // scalastyle:ignore null
      }
    }
}
