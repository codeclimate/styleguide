# Ruby

## RuboCop

Every project should start with the `rubocop.yml` present here. It should
enforce the styles defined here.

It should be pulled in via [external configuration].

### Non-Rails ruby code bases

```
prepare:
  fetch:
  - url: "https://raw.githubusercontent.com/codeclimate/styleguide/master/ruby/rubocop.yml"
    path: "base_rubocop.yml"

engines:
  rubocop:
    enabled: true
```

And the project should have a `.rubocop.yml` that looks like:

```
inherit_from: base_rubocop.yml

# project-specific configuration...
```

If a change or addition comes up in the course of that project that is not
project-specific, it should be made in the styleguide.

### Rails code bases

If the project is a Rails app, it should pull in the Rails config file as well:

```
prepare:
  fetch:
  - url: "https://raw.githubusercontent.com/codeclimate/styleguide/master/ruby/rubocop.yml"
    path: "base_rubocop.yml"
  - url: "https://raw.githubusercontent.com/codeclimate/styleguide/master/ruby/rails_rubocop.yml"
    path: "rails_rubocop.yml"

engines:
  rubocop:
    enabled: true
```

And the project should have a `.rubocop.yml` that looks like:

```
inherit_from: rails_rubocop.yml

# project-specific configuration...
```

## Reek

Every project should start with the `reek.yml` present here. It should
enforce the styles defined here.

It should be pulled in via [external configuration].

```
prepare:
  fetch:
  - url: "https://raw.githubusercontent.com/codeclimate/styleguide/master/ruby/reek.yml"
    path: ".reek.yml"

engines:
  reek:
    enabled: true
```

Reek does not support config inheritance, so you will not add a
project-specific file.

Reek has strong opinions about object-orientation that many find
overaggressive in some parts of codebases where OO design is less
important. You may find these common exclusions to be helpful:

```
engines:
  reek:
    enabled: true
  exclude_paths:
    - spec/
    - app/helpers
    - app/controllers
```

## General

- Align `private`, `protected`, etc with other definitions (do not out-dent)
- Avoid explicit `return`s
- Avoid postfix conditionals
- Avoid ternary operators
- Define class methods with `def self.method_name`
- Do not use an inner class outside of the class that defines it
- Define classes with the following structure (comments are for the clarity of
  the example and are not required):

  Example

  ```rb
  class User
    # Constants
    TIME_ALLOWED_INACTIVE = 10.minutes

    # Class method calls / DSL calls
    attr_reader :name, :address

    # Class method definitions
    def self.create(attrs)
      # ...
    end

    # Instance methods
    def send_email(email)
      # ...
    end

    # protected methods
    protected

    def protected_call_here
    end

    # private methods
    private

    def private_call_here
    end

    # Inner classes
    FakeUserError = Class.new(StandardError)

    class InnerClassMagic
    end
  end
  ```

- Don't align tokens

  ```rb
  # Bad, adding, removing, or changing values will be both annoying and produce
  # a noisy diff if it requires re-alignment
  foo       = "foo"
  bar_thing = "thing"
  other     = "other"

  {
    foo:       "foo"
    bar_thing: "thing"
    other:     "other"
  }

  # Good
  foo = "foo"
  bar_thing = "thing"
  other = "other"

  {
    foo: "foo"
    bar_thing: "thing"
    other: "other"
  }
  ```

- Don't use `self` unless required (`self.class` or attribute assignment)
- Don't use redundant braces when passing hash arguments

  ```rb
  # Bad
  Model.insert({ attr: "value" })

  # Good
  Model.insert(attr: "value")
  ```

- Prefer `Hash#fetch` when a key is required, or defaulted

  ```rb
  # Bad, returns "bar" even for { bar: nil } or { bar: false }
  foo[:bar] || "bar"

  # Good, returns "bar" only if :bar is not present
  foo.fetch(:bar) { "bar" }

  # Bad, may result in a NoMethodError for NilClass far, far away
  foo[:bar]

  # Good, will result in a KeyError right here
  foo.fetch(:bar)
  ```

- Sort all lists at the time of declaration (array items, hash keys, `require`
  statements, etc.)
- Use `%r{ }` for regular expressions containing more than one `/`
- Use `%w[ ]` for word-arrays
- Use `%{ }` for strings containing more than one double quote
- Use `do`/`end` for multi-line blocks and `{ }` for single-line blocks
- Use a trailing comma in all lists

  ```rb
  # Bad, adding a new entry requires a modification and addition
  [
    foo,
    bar,
    baz
  ]

  # Good, adding a new entry requires only an addition
  [
    foo,
    bar,
    baz,
  ]
  ```

- Use double quotes for all strings
- Use parentheses when calling methods with arguments, with the following
  exceptions: `puts`, `p`, `raise`, and class macros
- Use parentheses when defining methods that take arguments
- Don't use `unless` with an `else` branch. Switch the conditionals.
- Do, or do not. There is no `try`.
- Define error classes with `Class.new` where no subclass behavior is required:

  ```rb
  TerribleMorningException = Class.new(StandardError)
  ```

- Don't access instance variables directly outside of `#initialize`, (i.e define
  private `attr_reader`s as needed)

  The upcase forums has a good discussion [here][forum]. Ultimately, we choose
  this for the following:

  - Typo protection (typoed `@ivar` is `nil`, typoed `#ivar` is
    `NoMethodError`). Such a typo has actually caused a production incident.
  - Forcing mechanism against adding mutation by requiring an explicit writer or
    accessor be added to accomplish it.

  [forum]: https://forum.upcase.com/t/using-instance-variables-vs-attribute-accessors/1788/12

- Break long argument lists between every argument
- Break long method chains after the dot

## Namespaces

- Services (a unit of code that runs as a process) should use a
  `CC::{service_name}` namespace
- Library code which is domain specific (e.g. a Code Climate API or reads Code
  Climate config files) should use a `CC::{library_name}` namespace.
- Other library code (a gem or helper library) generally does not need to be
  namespaced (e.g. `GitClient` or `Minidoc`)

## Project structure

- Include a `.ruby_version`
- Include a `bin/setup` script
- Mirror `lib` in `spec`: `lib/foo/bar.rb => spec/foo/bar_spec.rb`

## Specs

**A note about the RSpec DSL**: we have rules below about avoiding some of
RSpec's DSL methods like `let`, `subject`, etc. These DSL methods can be easily
abused to result in [obfuscated and brittle tests][lets-not], hence the
*Avoid*/*Prefer* rules. That said, they can be fine to use in many
circumstances, therefore some clarification of the nuance involved is
worthwhile.

[lets-not]: https://robots.thoughtbot.com/lets-not

The overall guiding principle behind these rules is the following:

A developer should be able to view any `it` block in isolation --without its
`context`, without its `before`, without any `let`s it uses-- and understand
**exactly** what's going on. If you can accomplish this while using the RSpec
DSL methods, it's probably fine.

- Avoid `let`, and `subject` (prefer factory methods)
- Omit parenthesis with `#to` and `#not_to`

  ```rb
  # Good
  expect(x).to eq(y)

  # Bad, interrupts the English "to equal" phrase
  expect(x).to(eq(y))

  # Bad, doesn't always parse correctly
  expect(x).to eq y
  ```

- Place `describe` within the namespace(s) for inner classes
- Prefer `expect` syntax when possible
- Prefer predicate matchers when it reads well

  ```rb
  # Good
  expect(config).to be_valid_for_analysis

  # Bad
  expect(config.valid_for_analysis?).to be true

  # But sometimes required
  expect(presenter.show_the_thing?).to be true

  # Because this doesn't work
  expect(presenter).to be_show_the_thing
  ```

- Prefer spies to mocks when possible (mocks put assertion before action)
- Test only one thing per example in unit specs
- Use `match_array` when order doesn't matter
- Use `not_to` (not `to_not`, which creates a split-infinitive)
- Use a nested `describe` for each method (named as `"#foo"`, or `".foo"`)
- Write 4-phase tests with whitespace separating each phase

## Line length

- Prefer lines no longer than 80 characters

## Whitespace

- No trailing whitespace
- Use 2-space indentation

## Editor configuration

### vimrc

```vim
set colorcolumn=+1
set textwidth=80
set expandtab
set list listchars=tab:»·,trail:·
set shiftwidth=2
```

### sublime

```
"rulers": [ 80 ]
```

[external configuration]: https://docs.codeclimate.com/v1.0/docs/configuring-the-prepare-step
