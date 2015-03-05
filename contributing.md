---
layout: default
title:  "Contributing"
section: "contributing"
---
Discussion around Cats is currently happening in the
[Gitter channel](https://gitter.im/non/cats) as well as on Github
issue and PR pages. You can get an overview of who is working on what
via [Waffle.io](https://waffle.io/non/cats).

Feel free to open an issue if you notice a bug, have an idea for a
feature, or have a question about the code. Pull requests are also
gladly accepted. For more information, check out the [contributor guide](CONTRIBUTING.md).

People are expected to follow the
[Typelevel Code of Conduct](http://typelevel.org/conduct.html) when
discussing Cats on the Github page, Gitter channel, or other
venues.

We hope that our community will be respectful, helpful, and kind. If
you find yourself embroiled in a situation that becomes heated, or
that fails to live up to our expectations, you should disengage and
contact one of the [project maintainers](#maintainers) in private. We
hope to avoid letting minor aggressions and misunderstandings escalate
into larger problems.

If you are being harassed, please contact one of [us](#maintainers)
immediately so that we can support you.

## Contributing to this documentation

### source for documentation
The documentation for this website is stored alongside the source, in the [docs subproject](https://github.com/non/cats/tree/master/docs).

* The source for the static pages is in `docs/src/site`
* The source for the tut compiled pages is in `docs/src/main/tut`

### Generating the Site

run `sbt docs/makeSite`

### Previewing the site

1. Install jekyll locally, depending on your platform, you might do this with:

    yum install jekyll

    apt-get install jekyll

    gem install jekyll

2. In a shell, navigate to the generated site directory in `docs/target/site`

3. Start jekyll with `jekyll serve`

4. Navigate to http://localhost:4000/indoctrinate/ in your browser

5. Make changes to your site, and run `sbt makeSite` to regenerate the site. The changes should be reflected as soon as you run `makeSite`.

### Compiler verified documentation

We use [tut](https://github.com/tpolecat/tut) to compile source code
which appears in the documentation, this ensures us that our examples
should always compile, and our documentation has a better chance of
staying up-to-date.

### Publishing the site to github.

The `git.remoteRepo` variable in `docs/build.sbt` controls which
repository you will push to. Ensure that this variable points to a
repo you wish to push to, and that it is one for which you have push
access, then run `sbt ghpagesPushSite`

