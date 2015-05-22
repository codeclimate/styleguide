# Code Climate Style Guide

A consistent style is important in a code base. Inconsistency greatly increases
the number of inconsequential decisions developers must make. Consistency is
good for readability and a clean code base promotes keeping a code base clean
(i.e. Broken Window Theory). It is more important, and ultimately will make you
happier, to abide by a defined style and end up in a consistent code base, than
to code in your own preferred style and not.

Any PR comments regarding violations of the guidelines here should be addressed
immediately and without discussion. If you disagree with a guideline, propose a
change in a PR. Once the team has settled on a guideline, it must be followed.

Existing code may be fixed for style if, and only if, it must be touched in the
course of your current work. Do not change code *only* to fix style.

## Terminology

- **Do**, **don't**, etc: these must be followed always
- **Avoid**/**prefer**: if not following these, the burden is on you to convince
  your reviewer why

## General

- Align `private`, `protected`, etc with other definitions (do not out-dent)
- Avoid explicit `return`s
- Avoid postfix conditionals
- Avoid ternary operators
- Define class methods with `def self.method_name`
- Do not use an inner class outside of the class that defines it
- Define classes with the following structure (comments are for the clarity of example and are not required):

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

## Project structure

- Include a `.ruby_version`
- Include a `bin/setup` script
- Mirror `lib` in `spec`: `lib/foo/bar.rb => spec/foo/bar_spec.rb`

## Specs

- Avoid `described_class`
- Avoid `let`, and `subject` (prefer factory methods)
- Place `describe` within the namespace(s) for inner classes
- Prefer `expect` syntax when possible
- Prefer spies to mocks when possible (mocks put assertion before action)
- Test only one thing per example in unit specs
- Use a nested `describe` for each method (named as `"#foo"`, or `".foo"`)
- Write 4-phase tests with whitespace separating each phase

## Line length

- Break long argument lists between every argument
- Break long method chains after the dot

## Whitespace

- No trailing whitespace
- Use 2-space indentation

**vimrc**

```vim
set expandtab
set list listchars=tab:»·,trail:·
set shiftwidth=2
```

- No blank lines after opening or before closing a block
- No blank lines after starting or before closing a class or module definition
