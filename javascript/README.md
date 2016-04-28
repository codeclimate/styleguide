# JavaScript

## ESLint

This repo contains an `eslintrc.js` file that should be included (as
`.eslintrc.js`) in each project. Periodically, a repo's `.eslintrc.js` file
should be synced with the one here.

### Variable and Function Names

Names of variables and functions should be CamelCased. This may not be
practical to enforce automatically with ESLint on all projects, since code
interacting with APIs will frequently need to snake\_case object properties for
API queries and such. In those cases, only use snake\_case when interacting
directly with the API, either querying or handling a response. If an API
response is being transformed into a model-like object, key names should change
to CamelCase at that boundary. E.g.:

```
api.getModel("id").then(function(data) {
  var model = new Model();
  model.keyName = data.key_name;
  return model;
});
```

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

### ECMAScript 6

There is an additional set of ESLint rules we use for ES6 code: these are in
`eslintrc-es6.js`, and can be included into the main `.eslintrc.js` file in ES6
projects with an `"extends"` directive.

The rules there are mostly self explanatory, with one perhaps requiring an
explanatory note: `=>` functions are preferred over the `var self = this;`
pattern, but not in all cases, as there are good reasons *not* to use `=>`
sometimes.  The `consistent-this` rule makes it feasible to flag these
non-preferred usages without causing false positives on other cases, though
this isn't the strictly intended purpose of the rule. This does not mean that
`=>` should not be used in any other cases, only that whether to use it or not
in other cases is a judgment call.
