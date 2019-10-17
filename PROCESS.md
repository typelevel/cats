## Process

### Introduction

This document is intended to help consolidate the practices we are
using to develop and maintain Cats. Its primary audience is Cats
maintainers who may need to close issues, merge PRs, do releases, or
understand how these things work.

### Merging pull requests

Pull requests currently require two sign-offs from Cats
maintainers. Community member sign-offs are appreciated as votes of
confidence but don't usually count toward this total unless the
commenter has already been involved with the area of code in question.

When fixing typos, improving documentation or minor build fix only 
one sign-off is required (although for major edits waiting for two
 may be preferable).

For serious emergencies or work on the build which can't easily be
reviewed or tested, pushing directly to master may be OK (but is
definitely not encouraged). In these cases it's best to comment in
Gitter or elsewhere about what happened and what was done.

### Versioning

Since `1.0.0` release, Cats adopted the *MAJOR.MINOR.PATCH* 
[Semantic Versioning 2.0.0](http://semver.org/). In semantic versioning,
Backward binary compatibility is maintained between *PATCH* AND *MINOR* versions.

The main rules are:  
* *PATCH* version Z (x.y.Z | x > 0) MUST be incremented if only backwards compatible bug fixes are introduced. 
* *MINOR* version Y (x.Y.z | x > 0) MUST be incremented if new, binary backwards compatible functionality is introduced to the public API. It MUST be incremented if any public API functionality is marked as deprecated. 
* Source breaking but binary compatible changes are allowed between *MINOR* versions.  
* Binary backward breaking changes are **ONLY** allowed between *MAJOR* versions. 
* For other scenarios, refer to [Semantic Versioning 2.0.0](http://semver.org/).


For a new *MINOR* version release, it's preferred to release a Release Candidate for public testing. If there are no regressions or new bugs discovered, the new *MINOR* version should be released within a couple of weeks, with no significant changes in between.    

 
### Pre-release

Before the actual release, we need to make sure all merged PRs are properly assigned with the correct Github Milestone and labels. 

1. Make sure a milestone corresponding to the to-be released version exists on Github. 

2. Then use the following search to gather all merged PRs to be released: 
https://github.com/typelevel/cats/pulls?utf8=%E2%9C%93&q=merged%3A%3E2019-05-29+
replace `2019-05-29` with the last release date.

3. For PRs that add no value to the code or documentation or build, for example, community announcements or additions to adopter list, we do not include them in the release notes. Assign these PRs a special milestone: `Excluded from release notes` 

4. Assign the rest PRs with the target milestone and one or more of the following labels: `testing`, `bug`, `build`, `documentation`, `enhancement`, `Source Breaking` and `Binary Breaking`.

#### Release branch

For non-milestone releases (e.g. 2.0.0-M1), we shall release from a release branch. For each *MINOR* version, we shall have a corresponding branch, e.g. `2.1.x`. There are 2 main benefits for this: 
1. Since we need to go through at least 1 release candidate release, having a release branch makes it easier to incorporate potential fixes and release the official release later. 
2. The master branch of Cats is protected. This means sbt-release cannot push post release commits and more importantly tags directly to master. It can with a release branch, and a PR can be submitted to merge the release branch back into master. 

  
### Releasing

Before the release, the tests and other validation must be passing.

Currently, the best way to release Cats is:

 1. Run `+ clean` to ensure a clean start.
 2. Run `+ package` to ensure everything builds.
 3. Run `release`, which will validate tests/code, etc.

(Eventually `release` should do all of these things but for now the
individual steps are necessary.)

You will need to enter a GPG password to sign the artifacts correctly,
and you will also need to have Sonatype credentials to publish the
artifacts. The release process should take care of everything,
including releasing the artifacts on Sonatype.

If the Sonatype publishing fails, but the artifacts were uploaded, you
can finish the release manually using the Sonatype web site. In that
case you will also need to do `git push --tags origin master` to push
the new version's tags.

(In the future we may want to create branches for major versions,
e.g. `v1.x`. For now we don't have these.)

The release will take a few hours to sync to
[Maven Central](http://search.maven.org/). It's usually better to hold
off announcing the release until this sync has happened. (Before the
sync, users may not be able to resolve the new version.)

### Post-release

After the release occurs, you will need to update the
documentation. Here is a list of the places that will definitely need
to be updated:

 * `README.md`: update version numbers
 * `AUTHORS.md`: add new contributors
 * `CHANGES.md`: summarize changes since last release

(Other changes may be necessary, especially for large releases.)

If the [Pre-release](#pre-release) step is properly done, meaning all released PRs are properly assigned with label and milestone, you can use a [script](
https://github.com/typelevel/cats/blob/master/scripts/releaseNotes.scala) to generate the release note. Follow the doc in the script for instructions.  

Alternatively, you can get a list of changes between release tags `v0.1.2` and
`v0.2.0` via `git log v0.1.2..v0.2.0`. Scanning this list of commit
messages is a good way to get a summary of what happened, and manually write it up.

Once the relevant documentation changes have been committed, new
[release notes](https://github.com/typelevel/cats/releases) should be
added. You can add a release by clicking the "Draft a new release" button
on that page, or if the relevant release already exists, you can click
"Edit release".

The website should then be updated via [sbt-microsites](https://47deg.github.io/sbt-microsites/)
using `sbt docs/publishMicrosite`. 
Please check the [prerequisites](https://47deg.github.io/sbt-microsites/docs/) of `sbt-microsites`.

### Conclusion

Ideally this document will evolve and grow as the project
matures. Pull requests to add sections explaining how maintenance
occurs (or should occur) are very welcome.
