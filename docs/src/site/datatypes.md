---
layout: default
title:  "Data Types"
section: "data"
---
# Data Types

{% for x in site.tut %}
  {% if x.section == 'data' %}
- [{{x.title}}]({{site.baseurl}}{{x.url}})
  {% endif %}
{% endfor %}
