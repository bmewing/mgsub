has_filter_overlap_native = function() {
  is.loaded("_mgsub_filter_overlap_cpp", PACKAGE = "mgsub")
}

has_get_matches_native = function() {
  is.loaded("_mgsub_get_matches_cpp", PACKAGE = "mgsub")
}

has_collect_matches_native = function() {
  is.loaded("_mgsub_collect_matches_cpp", PACKAGE = "mgsub")
}

has_resolve_matches_native = function() {
  is.loaded("_mgsub_resolve_matches_cpp", PACKAGE = "mgsub")
}

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
  if (has_get_matches_native()) {
    return(.Call("_mgsub_get_matches_cpp", string, pattern, i, list(...),
                 PACKAGE = "mgsub"))
  }

  warn_native_fallback("get_matches")
  get_matches_base(string, pattern, i, ...)
}

collect_matches = function(string, pattern, ...) {
  if (length(pattern) == 0) {
    return(collect_matches_base(string, pattern, ...))
  }

  if (has_collect_matches_native()) {
    return(.Call("_mgsub_collect_matches_cpp", string, pattern, list(...),
                 PACKAGE = "mgsub"))
  }

  warn_native_fallback("collect_matches")
  collect_matches_base(string, pattern, ...)
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
  if (has_filter_overlap_native()) {
    return(.Call("_mgsub_filter_overlap_cpp", x, PACKAGE = "mgsub"))
  }

  warn_native_fallback("filter_overlap")
  filter_overlap_base(x)
}

resolve_matches = function(x) {
  if (has_resolve_matches_native()) {
    return(.Call("_mgsub_resolve_matches_cpp", x, PACKAGE = "mgsub"))
  }

  warn_native_fallback("resolve_matches")
  resolve_matches_base(x)
}
