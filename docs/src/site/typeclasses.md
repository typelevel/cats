---
layout: default
title:  "Type Classes"
section: "typeclasses"
groups:
  - Group 1:
    - Functor
    - Apply
    - Applicative
    - Monad
    - MonadCombine
    - MonadFilter
  - Group 2:
    - Semigroup
    - Monoid
    - SemigroupK
    - MonoidK
  - Group 3:
    - Foldable
    - Traverse
  - Misc.:
    - Contravariant
    - Invariant
    - InvariantMonoidal
    - Show
---
{% include_relative _tut/typeclasses.md %}

{% for groups_hash in page.groups %}
  {% for group in groups_hash %}
- {{group[0]}}
    {% for name in group[1] %}
  - [{{name}}]({{site.baseurl}}/typeclasses/{{name | downcase}}.html)
    {% endfor %}
  {% endfor %}
{% endfor %}

