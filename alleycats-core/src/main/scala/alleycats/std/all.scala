package alleycats
package std

import export._

@reexports(
  EmptyKInstances,
  ListInstances,
  OptionInstances,
  SetInstances,
  TryInstances,
  IterableInstances,
  FutureInstances
) object all extends LegacySetInstances with LegacyTryInstances with LegacyIterableInstances with MapInstances
