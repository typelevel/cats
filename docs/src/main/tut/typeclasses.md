---
layout: docs
title:  "Type Classes"
section: "typeclasses"
position: 1
---
{% include_relative typeclasses/typeclasses.md %}

{% for x in site.pages %}
{% if x.section == 'typeclasses' %}
- [{{x.title}}]({{site.baseurl}}{{x.url}})
{% endif %}
{% endfor %}

