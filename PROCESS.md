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

When fixing typos or improving documentation only one sign-off is
required (although for major edits waiting for two may be preferable).

For serious emergencies or work on the build which can't easily be
reviewed or tested, pushing directly to master may be OK (but is
definitely not encouraged). In these cases it's best to comment in
Gitter or elsewhere about what happened and what was done.

### Versioning

If a release is simply a bug fix, increment the patch version number
(e.g. 1.2.3 becomes 1.2.4). These releases may happen quite quickly in
response to reported bugs or problems, and should usually be source
and binary compatible.

If the major version is 0, then the minor version should be updated
(e.g. 0.2.3 becomes 0.3.0). There are no compatibility guarantees for
this type of change.

If the major version is 1 or greater, then significant additions
should increment the minor version number (e.g. 1.2.3 becomes 1.3.0)
and breaking or incompatible changes should increment the major number
(e.g. 1.2.3 becomes 2.0.0). These major version bumps should only
occur after substantial review, warning, and with proper deprecation
cycles.

### Releasing

Before the release, the tests and other validation must be passing.

Currently, the best way to release cats is:

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

You can get a list of changes between release tags `v0.1.2` and
`v0.2.0` via `git log v0.1.2..v0.2.0`. Scanning this list of commit
messages is a good way to get a summary of what happened, although it
does not account for conversations that occurred on Github.

Once the relevant documentation changes have been committed, new
[release notes](https://github.com/typelevel/cats/releases) should be
added. You can add a release by clicking the "Draft a new release" button
on that page, or if the relevant release already exists, you can click
"Edit release".

The website should then be updated via [sbt-microsites](https://47deg.github.io/sbt-microsites/)
using `sbt docs/publishMicrosite`. 
Please check the [prerequisites](https://47deg.github.io/sbt-microsites/docs/) of sbt-microsites.

### Conclusion

Ideally this document will evolve and grow as the project
matures. Pull requests to add sections explaining how maintenance
occurs (or should occur) are very welcome.
