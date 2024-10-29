package alleycats
package syntax

import alleycats.Extract

object extract extends ExtractSyntax

trait ExtractSyntax {
  implicit class ExtractOps[F[_], A](fa: F[A])(implicit ev: Extract[F]) {
    def extract(): A = ev.extract(fa)
  }
}