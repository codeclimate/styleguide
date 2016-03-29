# Haskell

## Safety

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

## Project Structure

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

## Formatting

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

  **Reasoning**: in general, one should be able to rename an identifier without
  having to adjust indentation. Notice the following:

  ```hs
  -- Bad
  foo :: String
      -> Int
      -> Int
  foo = undefined

  -- If I rename foo to longFoo, I have to adjust all four lines
  longFoo :: String
          -> Int
          -> Int
  longFoo = undefined

  -- Good
  foo
      :: String
      -> Int
      -> Int
  foo = undefined

  -- If I rename foo to longFoo, I only have to adjust the unavoidable two
  longFoo
      :: String
      -> Int
      -> Int
  longFoo = undefined
  ```

  *Comma-first* style is recommended below for the same reason.

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
