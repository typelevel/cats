# Contributor guide

## How can I help?

Cats follows a standard
[fork and pull](https://help.github.com/articles/using-pull-requests/)
model for contributions via GitHub pull requests.

Below is a list of the steps that might be involved in an ideal
contribution. If you don't have the time to go through every step,
contribute what you can, and someone else will probably be happy to
follow up with any polishing that may need to be done.

If you want to touch up some documentation or fix typos, feel free to
skip these steps and jump straight to submitting a pull request.

 1. [Find something that belongs in cats](#find-something-that-belongs-in-cats)
 2. [Let us know you are working on it](#let-us-know-you-are-working-on-it)
 3. [Implement your contribution](#write-code)
 4. [Write tests](#write-tests)
 5. [Write documentation](#write-documentation)
 6. [Write examples](#write-examples)
 7. [Submit pull request](#submit-a-pull-request)

### Find something that belongs in cats

Looking for a way that you can help out? Check out our
[Waffle.io page](https://waffle.io/non/cats). Choose a card from the
"Ready" column. Before you start working on it, make sure that it's
not already assigned to someone and that nobody has left a comment
saying that they are working on it!

(Of course, you can also comment on an issue someone is already
working on and offer to collaborate.)

Have an idea for something new? That's great! We recommend that you
make sure it belongs in cats before you put effort into creating a
pull request. The preferred ways to do that are to either:

 * [create a GitHub issue](https://github.com/non/cats/issues/new)
   describing your idea.
 * get feedback in the [cats Gitter room](https://gitter.im/non/cats).

Things that belong in cats generally have the following characteristics:

 * Their behavior is governed by well-defined [laws](laws).
 * They provide general abstractions.

Laws help keep types consistent, and remove ambiguity or sensitivity
about how particular instances can behave. We've found that types with
laws are often more useful than *lawless* types

(In some cases, *lawless* type classes and instances are useful. We
intend to support some of these in a future module.)

By staying general, Cats' abstractions are widely-applicable, and not
tied to particular libraries or strategies. Rather than being a
library to work with databases, HTTP requests, etc, Cats provides
abstractions used to build those libraries.

Cats (and especially `cats-core`) is intended to be lean and modular.
Some great ideas are not a great fit, either due to their size or
their complexity. In these cases, creating your own library that
depends on Cats is probably the best plan.

### Let us know you are working on it

If there is already a GitHub issue for the task you are working on,
leave a comment to let people know that you are working on it. If
there isn't already an issue and it is a non-trivial task, it's a good
idea to create one (and note that you're working on it). This prevents
contributors from duplicating effort.

### Write code

TODO

*Should this just link to a separate doc? This might get large.*

Write about implicit params as discussed in https://github.com/non/cats/issues/27

Write about type class methods on data structures as described in https://github.com/non/cats/issues/25

Write about https://github.com/non/cats/pull/36#issuecomment-72892359

### Write tests

TODO

Write about checking laws

### Write documentation

TODO

Write about ScalaDoc

Write about compilable documentation once https://github.com/non/cats/issues/17 is resolved

### Write examples

TODO

### Submit a pull request

Before you open a pull request, you should make sure that `sbt
validate` runs successfully. Travis will run this as well, but it may
save you some time to be alerted to style problems earlier.

If your pull request addresses an existing issue, please tag that
issue number in the body of your pull request or commit message. For
example, if your pull request addresses issue number 52, please
include "fixes #52".

## How did we do?

Getting involved in an open source project can be tough. As a
newcomer, you may not be familiar with coding style conventions,
project layout, release cycles, etc. This document seeks to demystify
the contribution process for the cats project.

It may take a while to familiarize yourself with this document, but if
we are doing our job right, you shouldn't have to spend months poring
over the project source code or lurking the
[Gitter room](https://gitter.im/non/cats) before you feel comfortable
contributing. In fact, if you encounter any confusion or frustration
during the contribution process, please create a GitHub issue and
we'll do our best to improve the process.
