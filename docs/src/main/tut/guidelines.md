---
layout: page
title:  "Guidelines"
section: "guidelines"
position: 7
---

# Guidelines

All guidelines in cats should have clear justifications. There is no room for tribal wisdom in a simple library.

## Syntax

### Composing Implicit Conversions in Traits

Implicit syntax conversions provided in publicly-exposed traits should be marked final 
so that any composition of the traits provides conversions that can all be inlined.

### Ops Classes

Ops classes should be marked final and extend AnyVal, to take full advantage of inlining and prevent unnecessary allocations.

The most notable exception is the case where all of the ops in the class are provided by zero-cost macros anyway,
for example with Simulacrum.

#### TODO: 
Once we drop 2.10 support, AnyVal-extending class constructor parameters can be marked as private.