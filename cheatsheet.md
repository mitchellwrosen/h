### awk

-----

#### Select a field: `{print $1}`

```haskell
\x -> words x !! 0
```

#### Format field(s): `{print $1 "\t" $2}`

```haskell
\(map unpack . words -> x) -> pack $ printf "%s\t%s" (x!!0) (x!!1)
```

#### Double space a file: `1; {print ""}`

```haskell
\x -> [x, ""]
````

#### Preceding line numbers: `{print NR " " $0}`

(`awk` streams this, `h` does not)

```haskell
\xs -> map (\(n,x) -> pack $ show n ++ " " ++ unpack x) (zip [1..] xs)
```

#### Count lines: `END{print NR}`

```haskell
length
```

#### Sum all numbers in each line: `{s=0; for (i=1; i<=NF; i++) s=s+$i; print s}`

```haskell
\(T.words -> xs) -> sum [ n :: Int | [(n,"")] <- map (reads . unpack) xs ]
```

## Add more here!
