# Filter overlaps from matches

Helper function used to identify which results from gregexpr overlap
other matches and filter out shorter, overlapped results

## Usage

``` r
filter_overlap(x)
```

## Arguments

- x:

  Matrix of gregexpr results, 4 columns, index of column matched, start
  of match, length of match, end of match. Produced exclusively from a
  worker function in mgsub
