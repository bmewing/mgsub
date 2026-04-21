# Fast escape replace

Fast escape function for limited case where only one pattern provided
actually matches anything

## Usage

``` r
fast_replace(string, pattern, replacement, ...)
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

  arguments to pass to gsub
