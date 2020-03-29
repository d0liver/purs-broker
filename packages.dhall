{- Welcome to your new Dhall package-set!

Below are instructions for how to edit this file for most use
cases, so that you don't need to know Dhall to use it.

## Warning: Don't Move This Top-Level Comment!

Due to how `dhall format` currently works, this comment's
instructions cannot appear near corresponding sections below
because `dhall format` will delete the comment. However,
it will not delete a top-level comment like this one.

## Use Cases

Most will want to do one or both of these options:
1. Override/Patch a package's dependency
2. Add a package not already in the default package set

This file will continue to work whether you use one or both options.
Instructions for each option are explained below.

### Overriding/Patching a package

Purpose:
- Change a package's dependency to a newer/older release than the
    default package set's release
- Use your own modified version of some dependency that may
    include new API, changed API, removed API by
    using your custom git repo of the library rather than
    the package set's repo

Syntax:
Replace the overrides' "{=}" (an empty record) with the following idea
The "//" or "⫽" means "merge these two records and
  when they have the same value, use the one on the right:"
-------------------------------
let overrides =
  { packageName =
      upstream.packageName // { updateEntity1 = "new value", updateEntity2 = "new value" }
  , packageName =
      upstream.packageName // { version = "v4.0.0" }
  , packageName =
      upstream.packageName // { repo = "https://www.example.com/path/to/new/repo.git" }
  }
-------------------------------

Example:
-------------------------------
let overrides =
  { halogen =
      upstream.halogen // { version = "master" }
  , halogen-vdom =
      upstream.halogen-vdom // { version = "v4.0.0" }
  }
-------------------------------

### Additions

Purpose:
- Add packages that aren't already included in the default package set

Syntax:
Replace the additions' "{=}" (an empty record) with the following idea:
-------------------------------
let additions =
  { package-name =
       { dependencies =
           [ "dependency1"
           , "dependency2"
           ]
       , repo =
           "https://example.com/path/to/git/repo.git"
       , version =
           "tag ('v4.0.0') or branch ('master')"
       }
  , package-name =
       { dependencies =
           [ "dependency1"
           , "dependency2"
           ]
       , repo =
           "https://example.com/path/to/git/repo.git"
       , version =
           "tag ('v4.0.0') or branch ('master')"
       }
  , etc.
  }
-------------------------------

Example:
-------------------------------
let additions =
  { benchotron =
      { dependencies =
          [ "arrays"
          , "exists"
          , "profunctor"
          , "strings"
          , "quickcheck"
          , "lcg"
          , "transformers"
          , "foldable-traversable"
          , "exceptions"
          , "node-fs"
          , "node-buffer"
          , "node-readline"
          , "datetime"
          , "now"
          ]
      , repo =
          "https://github.com/hdgarrood/purescript-benchotron.git"
      , version =
          "v7.0.0"
      }
  }
-------------------------------
-}


let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.6-20200226/packages.dhall sha256:3a52562e05b31a7b51d12d5b228ccbe567c527781a88e9028ab42374ab55c0f1

let overrides = {=}

let additions =
      { sequences =
          { dependencies =
            [ "prelude"
            , "unsafe-coerce"
            , "partial"
            , "unfoldable"
            , "lazy"
            , "arrays"
            , "profunctor"
            , "maybe"
            , "tuples"
            , "newtype"
            ]
          , repo = "https://github.com/hdgarrood/purescript-sequences"
          , version = "v2.1.0"
          }
      , in-memory-store =
          { repo = "https://github.com/d0liver/purs-in-memory-store.git"
          , version = "c8c0a178c26ed07d6a1f9ad8fb02794849575b53"
          , dependencies = [
              "aff"
            , "console"
            , "effect"
            , "foreign"
            , "foreign-generic"
            , "psci-support"
            , "spec"
            ]
          }
      , template-strings =
          { dependencies = [ "functions", "tuples" ]
          , repo =
              "https://github.com/purescripters/purescript-template-strings"
          , version = "v5.1.0"
          }
      , jquery-fancy =
          { dependencies = [ "prelude", "jquery" ]
          , repo = "https://github.com/colehaus/purescript-jquery-fancy"
          , version = "v2.0.0"
          }
      , unicode-prelude =
          { dependencies = [ "prelude" ]
          , repo = "https://github.com/vyorkin/purescript-unicode-prelude"
          , version = "v0.2.4"
          }
      }

in  upstream // overrides // additions
