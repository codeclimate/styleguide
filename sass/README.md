# [Sass](http://sass-lang.com)

Prefer the `.scss` syntax over the `.sass` syntax.

## scss-lint

Every project should start with the `.scss-lint.yml` present here. It hews
fairly closely to the default configuration with a few exceptions. The default
configuration is explained on the project's [linters page][0].

[0]: https://github.com/brigade/scss-lint/blob/master/lib/scss_lint/linter/README.md

Notable exceptions:

* use the [BEM][1] convention for selectors
* prefer double quotes

[1]: http://csswizardry.com/2013/01/mindbemding-getting-your-head-round-bem-syntax/
