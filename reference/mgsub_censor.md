# Safe, multiple censoring of text strings

`mgsub_censor` - A safe, simultaneous, multiple global string censoring
(replace matches with a censoring character like '\*')

## Usage

``` r
mgsub_censor(
  string,
  pattern,
  censor = "*",
  split = any(nchar(censor) > 1),
  seed = NULL,
  ...
)
```

## Arguments

- string:

  a character vector to censor

- pattern:

  regular expressions used to identify where to censor

- censor:

  character to use in censoring - see details

- split:

  if a multicharacter censor pattern is provided, should it be split to
  preserve original string length

- seed:

  optional parameter to fix sampling of multicharacter censors

- ...:

  arguments to pass to [`regexpr`](https://rdrr.io/r/base/grep.html) /
  [`sub`](https://rdrr.io/r/base/grep.html)

## Value

Censored string.

## Details

When censor is provided as a \>1 length vector or as a multicharacter
string with split = TRUE, it will be sampled to return random censoring
patterns. This can be helpful if you want to create cartoonish swear
censoring. If needed, the randomization can be controlled with the seed
argument.

## Examples

``` r
mgsub_censor("Flowers for a friend", pattern=c("low"), censor="*")
#> [1] "F***ers for a friend"
```
