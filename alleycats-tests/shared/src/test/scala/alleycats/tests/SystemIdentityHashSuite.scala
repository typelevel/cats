package alleycats.tests

import alleycats.SystemIdentityHash
import alleycats.laws.discipline.SystemIdentityHashTests
import cats.kernel.Hash

class SystemIdentityHashSuite extends AlleycatsSuite {
  implicit val hashObject: Hash[Object] = SystemIdentityHash[Object]

  checkAll("SystemIdentityHash[Object]", SystemIdentityHashTests[Object].hash)
}
