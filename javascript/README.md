# JavaScript

## ESLint

This repo contains an `eslintrc.js` file that should be included (as
`.eslintrc.js`) in each project. Periodically, a repo's `.eslintrc.js` file
should be synced with the one here.

### JQuery

- Prefer `return false` over `event.preventDefault()` when you don't need the
  event to bubble up.

  ```javascript
  // Bad
  $(".js-item").click(function(event) {
    event.preventDefault();
    $(this).hide();
  });

  // Good
  $(".js-item").click(function() {
    $(this).hide();
    return false;
  });
  ```
