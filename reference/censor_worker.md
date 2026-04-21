# mgsub_censor worker

The hard worker doing everything for mgsub_censor

## Usage

``` r
censor_worker(
  string,
  pattern,
  censor,
  split = any(nchar(censor) > 1),
  seed = NULL,
  ...
)
```

## Arguments

- string:

  a character vector where replacements are sought

- pattern:

  Character string to be matched in the given character vector

- censor:

  character to use in censoring - see details

- split:

  if a multicharacter censor pattern is provided, should it be split to
  preserve original string length

- seed:

  optional parameter to fix sampling of multicharacter censors

- ...:

  arguments to pass to regexpr family
