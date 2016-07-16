## Overview

*TL;DR: See `h --help`*

-----

`h` parses a Haskell function and applies it to stdin:

```
$ ls | h drop 2 | h T.map toUpper
PACKAGE.YAML
STACK.YAML
```

It uses the inferred type to determine whether you want to operate on the whole stream. **If you want to apply a function to each line, treat it as a `Text`, not a `String`!**

### Input types

The input type is inferred. If it is a **list**, it will be given all of stdin; if it is **not a list**, it will be given each line. The following input types are supported.

- `Text`
- `[Text]`
- `Read a => a`
- `Read a => [a]`

The `Read` instances, while useful for reading `Int`s and such, can also behave surprisingly. Consider:

```haskell
map toUpper :: [Char] -> [Char]
```

To `h`, the input is a `[ ]`, so each element will represent one line. `Char` is not `Text`, so we fall back to its `Read` instance. Thus, `h map toUpper` will read all of stdin into memory, try `read`ing it as a `[Char]`, and then apply `map toUpper` to it. Probably the user meant

```haskell
T.map toUpper :: Text -> Text
```

instead! (Or just `T.toUpper`).

Again, **if you want to apply a function to each line, treat it as a `Text`, not a `String`!**

### Output types

The output type is inferred as well. If it is a **list**, each element will be output on a separate line. The following output types are supported:

- `Text`
- `[Text]`
- `Bool`
- `Char`
- `Show a => a`
- `Show a => [a]`

Returning `Bool` will keep the line if `True` and omit it otherwise. To *literally* output the string `"True"` without quotes, use `Text`.

```
$ ls | h '\x -> T.length x > 10'
h-cli-tool.cabal
package.yaml
```

Returning `Char` will omit the `'` marks around the `Show` instance. Note that this is not the case for returning `[Char]`, which will fall back on the `Show Char` instance and print one `Char` per line.

```
$ ls | h '\x -> unpack x !! 0'
h
H
p
R
s
```

To *literally* output a line that looks like a Haskell list, make it a singleton list, so that the inner list is `Show`n.

```
$ echo "hi" | h '\x -> [x,x]'
hi
hi
$ echo "hi" | h '\x -> [[x,x]]'
["hi","hi"]
```

### Partial functions

Partial functions are fine; lines that throw exceptions will be omitted.

```
$ ls | h '\x -> unpack x !! 10'
.
m
```

### What's in scope?

```haskell
  {-# LANGUAGE ExtendedDefaultRules #-}
  {-# LANGUAGE OverloadedStrings    #-}
  {-# LANGUAGE ScopedTypeVariables  #-}
  {-# LANGUAGE ViewPatterns         #-}

  import Data.Char
  import Data.Either
  import Data.List   hiding (lines, unlines, unwords, words)
  import Data.Maybe
  import Data.Monoid
  import Data.Text   (Text, lines, pack, unlines, unpack, unwords, words)
  import Prelude     hiding (lines, unlines, unwords, words)
  import Text.Printf
  import qualified Data.Text as T
```

## GHC Compatibility

- GHC 7.10

## Building and installing

This isn't uploaded to Hackage yet (day old toy project), but I will can throw
it up soon if anyone is interested. For now,

```
git clone git@github.com:mitchellwrosen/h
cd h && stack install
```

## Caveats

This unfortunately requires a GHC installation and a package database with
`text` installed. Run `ghc-pkg list text` and confirm that it's installed. If
not, I recommend aliasing `h` as `stack --stack-yaml <some stack.yaml> exec h --`.
Make sure that `stack --stack-yaml <some stack.yaml> exec -- ghc-pkg list text`
shows a `text` in scope.
