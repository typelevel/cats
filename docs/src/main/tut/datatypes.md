---
layout: docs
title:  "Data Types"
section: "data"
position: 2
---
# Data Types

{% for x in site.pages %}
  {% if x.section == 'data' and x.title != page.title %}
- [{{x.title}}]({{site.baseurl}}{{x.url}})
  {% endif %}
{% endfor %}
