{% laika.excludeFromNavigation = true %}

# Either

The `Either` type is provided by the Scala standard library, but Cats provides [syntax enrichment](../jump_start_guide.md#either-helpers) and typeclass instances
to extend its functionality, including [Functor], [Applicative], [Monad] and 
[MonadError](../typeclasses/applicativemonaderror.md).

If you would like to accumulate errors instead of the short-circuiting behavior of `Either`, see the 
[Parallel] typeclass, or the [Validated] data type.
