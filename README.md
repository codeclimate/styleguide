# Code Climate Style Guide

A consistent style is important in a code base. Inconsistency greatly increases
the number of inconsequential decisions developers must make. Consistency is
good for readability and a clean code base promotes keeping a code base clean
(i.e. Broken Window Theory). It is more important, and ultimately will make you
happier, to abide by a defined style and end up in a consistent code base, than
to code in your own preferred style and not.

Any PR comments regarding violations of the guidelines here should be addressed
immediately and without discussion. If you disagree with a guideline, propose a
change in a PR to this repo. Once the team has settled on a guideline, it must
be followed.

Existing code may be fixed for style if, and only if, it must be touched in the
course of your current work. Do not change code *only* to fix style.

## Terminology

- **Do**, **don't**, etc: these must be followed always
- **Avoid**/**prefer**: if not following these, the burden is on you to convince
  your reviewer why

## Rubocop

Every project should start with the `rubocop.yml` present here. It should
enforce the styles defined here.

If a change or addition comes up in the course of that project that is not
project-specific, it should be made in both places. Periodically, projects must
"sync" their `.rubocop.yml` with the one present here.

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

- Avoid `described_class`
- Avoid `let`, and `subject` (prefer factory methods)
- Place `describe` within the namespace(s) for inner classes
- Prefer `expect` syntax when possible
- Prefer spies to mocks when possible (mocks put assertion before action)
- Test only one thing per example in unit specs
- Use a nested `describe` for each method (named as `"#foo"`, or `".foo"`)
- Write 4-phase tests with whitespace separating each phase

## Line length

- Prefer lines no longer than 80 characters

**vimrc**

```vim
set colorcolumn=+1
set textwidth=80
```

**sublime**

```
"rulers": [ 80 ]
```

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

## Haskell

### Safety

- Avoid partial functions (`head`, `read`, etc)

  ```hs
  -- Bad
  getEmail :: User -> IO Email
  getEmail user = do
      emails <- requestApi $ "/users/" <> userId user <> "/emails"

      return $ head emails

  -- Good
  import Data.Maybe (listToMaybe)

  getEmail :: User -> IO (Maybe Email)
  getEmail user = do
      emails <- requestApi $ "/users/" <> userId user <> "/emails"

      return $ listToMaybe emails

  -- Better
  getEmail :: User -> IO (Either String Email)
  getEmail user = do
      -- `try` replaces `IO` exceptions with an `Either` result
      emails <- try $ requestApi $ "/users/" <> userId user <> "/emails"

      return $ case emails of
          Right (e:_) -> Right e
          Right _ -> Left "User had no emails"
          Left ex -> Left $ "IO error get emails: " <> show ex
  ```

- Compile with `-Wall -Werror`
- Use `newtype`s when multiple concepts share primitive types

  ```hs
  -- Bad
  fetchWebPage :: String -> IO String
  fetchWebPage url = -- returns HTML body

  main = do
      body <- fetchWebPage "http://example.com"
      nonSense <- fetchWebPage body
      -- ^ Bug found at runtime

  -- Good
  newtype URL = URL { unURL :: String }
  newtype HTML = HTML { unHTML :: String }

  fetchWebPage :: URL -> IO HTML
  fetchWebPage = -- ...

  main = do
      body <- fetchWebPage $ URL "http://example.com"
      nonSense <- fetchWebPage body
      -- ^ Type error at compile time
  ```

### Project Structure

- Use [stack][], with the latest LTS snapshot resolver
- Use a structure like the *hspec* template:
  - Source files under `src/`
  - HSpec-based tests under `test/`
  - Executable defined at `app/Main.hs`
- For web projects, prefer [Yesod][]
- For options parsing, prefer [optparse-applicative][]
- Build docker images in two layers ([example][popeye-commit])

[stack]: http://docs.haskellstack.org/en/stable/README.html
[yesod]: http://yesodweb.com
[optparse-applicative]: https://hackage.haskell.org/package/optparse-applicative
[popeye-commit]: https://github.com/codeclimate/popeye/commit/dd0daf131877ad5340571d81edc8c7c9e9588a82

### Formatting

- Use four-space indentation except the `where` keyword which is indented two
  spaces

  ```hs
  outputStream commandId start = do
      outputs <- runDB $ selectList
          [OutputCommand ==. commandId]
          [Asc OutputCreatedAt, OffsetBy start]

      stop <- commandRunning

      unless stop $ do
          mapM_ sendText outputs

          outputStream commandId (start + length outputs)

    where
      commandRunning = runDB $ exists
          [ CommandId ==. commandId
          , CommandRunning ==. True
          ]
  ```

- Break long type signatures before separators

  ```hs
  exists
      :: ( MonadIO m
         , PersistQuery (PersistEntityBackend v)
         , PersistEntity v
         )
      => [Filter v]
      -> ReaderT (PersistEntityBackend v) m Bool
  exists = fmap (> 0) . count
  ```

- Use only one pragma statement per line.

  ```hs
  -- Bad
  {-# LANGUAGE OverloadedStrings, RecordWildCards #-}

  -- Also bad
  {-# LANGUAGE OverloadedStrings
             , RecordWildCards #-}

  -- Good
  {-# LANGUAGE OverloadedStrings #-}
  {-# LANGUAGE RecordWildCards #-}
  ```

- Use comma-first style exports, records, and lists.

  ```hs
  -- exports
  module Main
      ( MyType(..)
      , (&&^)
      , (||^)
      , exportedFunc1
      , exportedFunc2
      ) where

  -- imports
  import Network.HTTP.Conduit
      ( HttpException(..)
      , RequestBody(..)
      , Manager
      , httpLbs
      , parseUrl
      , responseBody
      , tlsManagerSettings
      )

  -- record data types
  data Person = Person
      { personName :: String
      , personAge :: Int
      }
      deriving Show

  -- sum types
  data HTTPStatus
      = OK
      | BadRequest
      | ServerError
      deriving Eq

  -- lists
  mappedThings f = map f
      [ aThing
      , anotherThing
      , lastThing
      ]
  ```

- Order export and import lists as types, operators, then functions and
  alphabetize each group (the *comma-first* example follows this)
