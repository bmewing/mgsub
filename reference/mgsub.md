# Safe, multiple gsub

`mgsub` - A safe, simultaneous, multiple global string replacement
wrapper that allows access to multiple methods of specifying matches and
replacements.

## Usage

``` r
mgsub(string, pattern, replacement, recycle = FALSE, ...)
```

## Arguments

- string:

  a character vector where replacements are sought

- pattern:

  Character string to be matched in the given character vector

- replacement:

  Character string equal in length to pattern or of length one which are
  a replacement for matched pattern.

- recycle:

  logical. should replacement be recycled if lengths differ?

- ...:

  arguments to pass to [`regexpr`](https://rdrr.io/r/base/grep.html) /
  [`sub`](https://rdrr.io/r/base/grep.html)

## Value

Converted string.

## Examples

``` r
mgsub("hey, ho", pattern = c("hey", "ho"), replacement = c("ho", "hey"))
#> [1] "ho, hey"
mgsub("developer", pattern = c("e", "p"), replacement = c("p", "e"))
#> [1] "dpvploepr"
mgsub("The chemical Dopaziamine is fake",
      pattern = c("dopa(.*?) ", "fake"),
      replacement = c("mega\\1 ", "real"),
      ignore.case = TRUE)
#> [1] "The chemical megaziamine is real"
```
