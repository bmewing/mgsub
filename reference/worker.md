# mgsub worker

The hard worker doing everything for mgsub

## Usage

``` r
worker(string, pattern, replacement, ...)
```

## Arguments

- string:

  a character vector where replacements are sought

- pattern:

  Character string to be matched in the given character vector

- replacement:

  Character string equal in length to pattern or of length one which are
  a replacement for matched pattern.

- ...:

  arguments to pass to regexpr family
