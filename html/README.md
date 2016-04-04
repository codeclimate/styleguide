# HTML

- Always use quotes when specifying attribute values.

  ```html
  <!-- Good -->
    <td class="foo"></td>
    <td class="foo bar"></td>

  <!-- Bad -->
    <td class=foo></td>
  ```

  Reasoning: since quotes are required in some situations, quote consistently
  to minimize thought points + diffs.
