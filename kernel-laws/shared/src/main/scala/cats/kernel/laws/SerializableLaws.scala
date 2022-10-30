/*
 * Copyright (c) 2015 Typelevel
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package cats.kernel.laws

import cats.platform.Platform
import org.scalacheck.Prop
import org.scalacheck.Prop.{Exception, Proof, Result}

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
  // `Platform.isJvm` is a constant expression, so we can rely on
  // scalac to prune away the "other" branch. Thus, when Scala.js
  // looks at this method it won't "see" the branch which was removed,
  // and will avoid an error trying to support java.io.*.

  def serializable[A](a: A): Prop =
    if (Platform.isJvm) {
      Prop { _ =>
        import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}

        val baos = new ByteArrayOutputStream()
        val oos = new ObjectOutputStream(baos)
        var ois: ObjectInputStream = null
        try {
          oos.writeObject(a)
          oos.close()
          val bais = new ByteArrayInputStream(baos.toByteArray())
          ois = new ObjectInputStream(bais)
          val _ = ois.readObject()
          ois.close()
          Result(status = Proof)
        } catch {
          case t if NonFatal(t) =>
            Result(status = Exception(t))
        } finally {
          oos.close()
          if (ois != null) ois.close()
        }
      }
    } else Prop(_ => Result(status = Proof))
}
