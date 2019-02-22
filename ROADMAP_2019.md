This roadmap is produced based on Cats ecosystem community survey 2018 results - See [#2719](https://github.com/typelevel/cats/issues/2719)

## Overall strategy
A Cats 2.0 is released earlier in the year **without any breaking changes on most modules** except `cats-laws` and `cats-testkit`. We then start branching to allow the master branch to drop Scala 2.11 in Q2, but maintain a dedicated `scala2_11` branch to continue support Scala 2.11 until near the end of 2019. During this period, we are going to backport incoming changes from `master` to the `scala2_11` branch in a Scala 2.11 BC way. We will try to maintain as much as possible use site source compatibility between on Scala 2.11 code and Scala 2.12+ - but they will be source incompatible - mostly methods added to traits in 2.12 will only be available in 2.11 code in new `XXXXBinCompatN` traits. During this period, we'll release Cats on scala 2.12+ and 2.11 from two different branches.

The followings are the steps in each quarter. 

## Q1    
#### Release 1.6 [already done]
#### Release 2.0: This is not the Cats 2.0 we’ve been discussing in the past. Most modules remain fully binary compatible with Cats 1.x on all Scala versions. The only breaking changes are: 
* Scalacheck 1.13, update to Scalacheck 1.14
* breaking changes for Cats-laws
						
## Q2 
Start a Scala 2.11 dedicated branching scheme: 
* Branch master - drops Scala 2.11, maintains is BC with 1.x
* Branch scala2_11 - Scala 2.11 code
New changes to master have to be ported to branch scala2_11 in a 2.11 BC compatible way and try to maintain as use site source compatible as possible - e.g. a new type class method added to master is added to a syntax trait method in scala2_11 branch.
### Release Cats 2.1 RC1 on both Scala 2.11 and Scala 2.12+
   Note that 2.11 and 2.12+ code are NOT source compatible with each other, however, they will still maintain source compatibility with 2.0 and 1.x.
### Release Cats 2.1 RC2
### Release Cats 2.1

## Q3

### Release Cats 2.2
### Release Cats 2.3

## Q4 
### Release Cats 2.4 - last release on Scala 2.11
### Stops actively maintaining scala2_11 branch 
### Release Cats 2.5 without scala 2.11
