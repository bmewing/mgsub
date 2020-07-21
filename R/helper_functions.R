fast_replace = function(string, pattern, replacement, ...) {
  #' @title Fast escape replace
  #'
  #' @description Fast escape function for limited case where only one pattern
  #' provided actually matches anything
  #'
  #' @param string a character vector where replacements are sought
  #' @param pattern Character string to be matched in the given character vector
  #' @param replacement Character string equal in length to pattern or of length
  #' one which are a replacement for matched pattern.
  #' @param \dots arguments to pass to gsub

  for (i in seq_along(pattern)) {
    string = gsub(pattern[i], replacement[i], string, ...)
  }
  return(string)
}

get_matches = function(string, pattern, i, ...) {
  #' @title Get all matches
  #'
  #' @description Helper function to be used in a loop to check each pattern
  #' provided for matches
  #'
  #' @param string a character vector where replacements are sought
  #' @param pattern Character string to be matched in the given character vector
  #' @param i an iterator provided by a looping function
  #' @param \dots arguments to pass to gregexpr

  tmp = gregexpr(pattern[i], string, ...)
  start = tmp[[1]]
  length = attr(tmp[[1]], "match.length")
  return(matrix(cbind(i, start, length, start + length - 1), ncol = 4))
}

filter_overlap = function(x) {
  #' @title Filter overlaps from matches
  #'
  #' @description Helper function used to identify which results from gregexpr
  #' overlap other matches and filter out shorter, overlapped results
  #'
  #' @param x Matrix of gregexpr results, 4 columns, index of column matched,
  #' start of match, length of match, end of match. Produced exclusively from
  #' a worker function in mgsub
  for (i in nrow(x):2) {
    s = x[i, 2]
    ps = x[1:(i - 1), 2]
    e = x[i, 4]
    pe = x[1:(i - 1), 4]
    if (any(ps <= s & pe >= s)) {
      x = x[-i, ]
      next
    }
    if (any(ps <= e & pe >= e)) {
      x = x[-i, ]
      next
    }
  }
  return(matrix(x, ncol = 4))
}
