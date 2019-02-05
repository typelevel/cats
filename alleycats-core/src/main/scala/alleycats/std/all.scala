package alleycats
package std

import export._

@reexports(EmptyKInstances,
           ListInstances,
           OptionInstances,
           SetInstances,
           TryInstances,
           IterableInstances,
           FutureInstances) object all
    extends LegacySetInstances
    with LegacySetInstancesBinCompat0
    with LegacyTryInstances
    with LegacyIterableInstances
    with MapInstances
