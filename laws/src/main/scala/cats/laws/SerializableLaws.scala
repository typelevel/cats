package cats
package laws

import java.io.{ ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream }

import org.scalacheck.Prop
import org.scalacheck.Prop.{ False, Proof, Result }

/**
 * Check for Java Serializability.
 */
object SerializableLaws {
  // scalastyle:off null
  def serializable[A](a: A): Prop =
    Prop { _ =>
      val baos = new ByteArrayOutputStream()
      val oos = new ObjectOutputStream(baos)
      var ois: ObjectInputStream = null
      try {
        oos.writeObject(a)
        oos.close()
        val bais = new ByteArrayInputStream(baos.toByteArray())
        ois = new ObjectInputStream(bais)
        val a2 = ois.readObject()
        ois.close()
        Result(status = Proof)
      } catch { case _: Throwable =>
        Result(status = False)
      } finally {
        oos.close()
        if (ois != null) ois.close()
      }
    }
  // scalastyle:on null
}
