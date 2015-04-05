---
layout: default
title:  "Typeclasses"
section: "typeclasses"
---
{% include_relative _tut/typeclasses.md %}

{% for x in site.tut %}
{% if x.section == 'typeclasses' %}
- [{{x.title}}]({{site.baseurl}}{{x.url}})
{% endif %}
{% endfor %}

