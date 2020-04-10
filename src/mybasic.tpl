{% extends 'basic.tpl' %}
{% block any_cell %}

{% if 'collapsed' in cell['metadata'].get('tags', []) %}
   {% set summary = cell['metadata'].get('collapsed-summary', "Collapsed IPython cell") %}
   <details class='cell-collapsed'>
   <summary>{{ summary }} <span class='cell-collapsed-expand'>(click to expand)</span></summary>
       {{ super() }}
   </details>
{% else %}
    {{ super() }}
{% endif %}

{% endblock any_cell %}
