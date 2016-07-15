## Overview

`h` parses a Haskell function and applies it to stdin. Some examples:

```
$ ls | h drop 2 | h T.map toUpper
PACKAGE.YAML
STACK.YAML

$ ls | h "T.intersperse ' '"
h - c l i - t o o l . c a b a l
H . h s
p a c k a g e . y a m l
s t a c k . y a m l
```

Note that it works on both single lines (as in `intersperse ' '`), and the whole input (as in `drop 2`).
Partial functions are okay, too:

```
$ cat nums
1
2
hi
3

$ cat nums | h show . subtract 1 . read . unpack
0
1
2
```

## Documentation


See `h --help`:

```
Execute an arbitrary Haskell function on stdin and write the results to stdout.
The function must have one of the following types:

  Text   -> Text
  Text   -> String
  Text   -> [Text]
  Text   -> [String]
  [Text] -> Text
  [Text] -> String
  [Text] -> [Text]
  [Text] -> [String]

The function is compiled in the following context:

  {-# LANGUAGE ExtendedDefaultRules #-}
  {-# LANGUAGE OverloadedStrings    #-}
  import Data.Char
  import Data.Either
  import Data.List   hiding (lines, unlines, unwords, words)
  import Data.Maybe
  import Data.Monoid
  import Data.Text   (Text, lines, pack, unlines, unpack, unwords, words)
  import Prelude     hiding (lines, unlines, unwords, words)
  import qualified Data.Text as T

Example usage:

  $ ls | h drop 2 | h T.map toUpper
  PACKAGE.YAML
  STACK.YAML

  $ ls | h "T.intersperse ' '"
  h - c l i - t o o l . c a b a l
  H . h s
  p a c k a g e . y a m l
  s t a c k . y a m l

Usage: h FUNCTION

Available options:
  -h,--help                Show this help text
  FUNCTION                 A Haskell function
```

## Building and installing

This isn't uploaded to Hackage yet (day old toy project), but I will can throw it up soon if anyone is interested. For now,

```
git clone git@github.com:mitchellwrosen/h
cd h && stack install
```

## Caveats

This unfortunately requires a GHC installation and a package database with `text` installed. Run `ghc-pkg list text` and
confirm that it's installed. If not, I recommend aliasing `h` as `stack --stack-yaml <some stack.yaml> exec h --`. 
Make sure that `stack --stack-yaml <some stack.yaml> exec -- ghc-pkg list text` shows a `text` in scope.
