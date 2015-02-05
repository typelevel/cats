# Contributor guide

## How can I help?

Cats follows a pretty standard [fork and pull](https://help.github.com/articles/using-pull-requests/) model for contributions via GitHub pull requests.

Below is a list of the steps that might be involved in an ideal contribution. If you don't have the time to go through every step, feel free to contribute what you can, and someone else will probably be happy to follow up with any polishing that may need to be done. If you want to touch up some documentation or fix typos, please feel free to skip all of these steps and jump straight to submitting a pull request.

1. Find something that belongs in cats - [details](#find-something-that-belongs-in-cats)
2. Let us know you are working on it - [details](#let-us-know-you-are-working-on-it)
3. Implement your contribution - [details](#write-code)
4. Write tests - [details](#write-tests)
5. Write documentation - [details](#write-documentation)
6. Write examples - [details](#write-examples)
7. Submit pull request - [details](#submit-a-pull-request)

### Find something that belongs in cats

Looking for a way that you can help out? Check out our [Waffle.io page](https://waffle.io/non/cats). Choose a card from the "Ready" column. Before you start working on it, make sure that it's not already assigned to someone and that nobody has left a comment saying that they are working on it!

Have an idea for something new? That's great! We recommend that you make sure it belongs in cats before you put effort into creating a pull request. Cats tries to stay pretty lean and modularized so it can be a lightweight dependency (especially for things like [Scala.js](http://www.scala-js.org/)), so even if your idea is very useful, cats may not be the right home for it. If you'd like to find out whether or not your idea is the right fit for cats before you put a lot of effort into it, you can run your idea by the people in the [cats Gitter room](https://gitter.im/non/cats) or [create a GitHub issue](https://github.com/non/cats/issues/new) describing your idea.

Things that belong in cats generally have the following characteristics:
* Their behavior is governed by well-defined [laws](laws).
  * This makes them consistent, easy to reason about, easy to test, and (most likely) more generally useful.
* They provide general abstractions
  * Everybody needs libraries that can talk to databases, make HTTP requests, etc. But cats gets to remain blissfully unaware of these specific tasks. It provides general abstractions that would likely be useful for someone _creating_ libraries to talk to databases, make HTTP requests, etc.

### Let us know you are working on it

If there is already a GitHub issue for the task you are working on, leave a comment to let people know that you are working on it. If there isn't already an issue and it is a non-trivial task, it may be a good idea to go ahead and create one with a message that you starting the work. This helps to prevent contributors from duplicating effort.

### Write code

TODO

Should this just link to a separate doc? This might get large.

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

If your pull request addresses an existing issue, please tag that issue number in the body of your pull request or commit message. For example, if your pull request addresses issue number 52, please include "fixes #52".

## How did we do?

Getting involved in an open source project can be tough. As a newcomer, you may not be familiar with coding style conventions, project layout, release cycles, etc. This document seeks to demystify the contribution process for the cats project. It may take a while to familiarize yourself with this document, but if we are doing our job right, you shouldn't have to spend months poring over the project source code or lurking the [Gitter room](https://gitter.im/non/cats) before you feel comfortable contributing. In fact, if you encounter any confusion or frustration during the contribution process, please create a GitHub issue and we'll do our best to improve the process.

