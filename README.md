# editor-config

[![Build Status](https://travis-ci.org/kseo/editor-config.svg?branch=master)](https://travis-ci.org/kseo/editor-config)

EditorConfig Haskell Core provides the same functionality as the [EditorConfig C
Core][core-c]. EditorConfig Haskell core can be used as a command line program
or as an importable library.

## EditorConfig Project

EditorConfig makes it easy to maintain the correct coding style when switching
between different text editors and between different projects. The EditorConfig
project maintains a file format and plugins for various text editors which allow
this file format to be read and used by those editors. For information on the
file format and supported text editors, see the [EditorConfig website][website].

## Installation

With cabal:

```
cabal install editor-config
```

## Getting Help
For help with the EditorConfig core code, please write to our [mailing
list][mailing-list]. Bugs and feature requests should be submitted to our [issue
tracker][issue-tracker].

## Using as a Library
Basic example use of EditorConfig Haskell Core as a library:

```haskell
import Data.EditorConfig

main = do
  res <- getProperties "a.py"
  case res of
    Left e   -> print e
    Right prop -> print (indentStyle prop)
```

[core-c]: https://github.com/editorconfig/editorconfig-core-c
[website]: http://editorconfig.org/
[mailing-list]: http://groups.google.com/group/editorconfig
[issue-tracker]: https://github.com/kseo/editor-config/issues
