package alleycats
package std

import export._

@reexports(
  EmptyKInstances,
  ListInstances,
  OptionInstances,
  SetInstances,
  TryInstances,
  IterableInstances
) object all extends LegacySetInstances with LegacyTryInstances with LegacyIterableInstances
