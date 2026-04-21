# Filter overlaps from matches using only base R functionality

Helper function using only base R functionality used to identify which
results from gregexpr overlap other matches and filter out shorter,
overlapped results

## Usage

``` r
filter_overlap_base(x)
```

## Arguments

- x:

  Matrix of gregexpr results, 4 columns, index of column matched, start
  of match, length of match, end of match. Produced exclusively from a
  worker function in mgsub
