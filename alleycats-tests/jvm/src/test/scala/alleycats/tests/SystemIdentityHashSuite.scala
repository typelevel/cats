package alleycats.tests

import alleycats.SystemIdentityHash
import cats.kernel.Hash
import cats.kernel.laws.discipline._

class SystemIdentityHashSuite extends AlleycatsSuite {
  implicit val hashObject: Hash[Object] = SystemIdentityHash[Object]

  checkAll("SystemIdentityHash[Object]", HashTests[Object].hash)
}
