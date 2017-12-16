Cats has been striving to provide functional programming abstractions that are core, modular, approachable and efficient. 
Cats 1.0.0 marks the point where we believe that our API is robust and stable enough to start guarantee backward binary compatibility going forward until Cats 2.0. We expect the Cats 1.x series to be fully backwards compatible for at least one year. This is a major milestone towards
our goal of providing a solid foundation for an ecosystem of pure, typeful functional libraries.

## Migration

The vast majority of changes since 1.0.0-RC1 are API compatible, with scalafix scripts ready for those that do not.
 [Here is the change list and migration guide](https://github.com/typelevel/cats/blob/master/CHANGES.md).

## Binary compatibility
After 1.0.0 release, we'll use the *MAJOR.MINOR.PATCH* [Semantic Versioning 2.0.0](https://semver.org/) going forward, which is different from the *EPOCH.MAJOR.MINOR* scheme common among Java and Scala libraries (including the Scala lang).  In this semantic versioning, backward breaking change is ONLY allowed between *MAJOR* versions. We will maintain backward binary compatibility between *PATCH* and *MINOR* versions. For example, when we release cats 1.1.0, it will be backward binary compatible with the previous 1.0.x versions. I.E. the new JAR will be a drop-in replacement for the old one. This is critical when your application has a diamond dependency on Cats - depending on two or more libraries that all depend on Cats. If one library upgrades to the new 1.1.0 Cats before the other one does, your application still runs thanks to this backward binary compatibility.

We will also consider using organization and package name for *MAJOR* versioning with binary breaking changes in the future. But that decision is yet to be made.

## Community
Cats is built for the FP Scala community by the FP Scala community. We can't thank enough to our [187 (and growing) contributors](https://github.com/typelevel/cats/graphs/contributors) and our users who provided feedbacks and suggestions.  
Congratulations to all of us. Let's celebrate this exciting milestone together. 
  
