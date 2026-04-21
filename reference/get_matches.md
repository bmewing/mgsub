# Get all matches

Helper function to be used in a loop to check each pattern provided for
matches

## Usage

``` r
get_matches(string, pattern, i, ...)
```

## Arguments

- string:

  a character vector where replacements are sought

- pattern:

  Character string to be matched in the given character vector

- i:

  an iterator provided by a looping function

- ...:

  arguments to pass to gregexpr
