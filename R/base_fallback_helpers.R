mgsub_runtime = new.env(parent = emptyenv())
mgsub_runtime$native_fallback_warned = FALSE

warn_native_fallback = function(function_name) {
  if (!isTRUE(getOption("mgsub.warn_native_fallback", TRUE)) ||
      isTRUE(mgsub_runtime$native_fallback_warned)) {
    return(invisible(NULL))
  }

  warning(
    paste(
      "Using the base R fallback.",
      "Rebuild or reinstall mgsub with compiled code enabled",
      "for better performance."
    ),
    call. = FALSE
  )
  mgsub_runtime$native_fallback_warned = TRUE
}

get_matches_base = function(string, pattern, i, ...) {
  #' @title Get all matches using only base R functionality
  #'
  #' @description Helper function to be used in a loop to check each pattern
  #' provided for matches when the compiled version isn't available
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

collect_matches_base = function(string, pattern, ...) {
  x = do.call(rbind, lapply(seq_along(pattern),
                            get_matches_base,
                            string = string,
                            pattern = pattern, ...))
  matrix(x[x[, 2] != -1, ], ncol = 4)
}

filter_overlap_base = function(x) {
  #' @title Filter overlaps from matches using only base R functionality
  #'
  #' @description Helper function using only base R functionality
  #' used to identify which results from gregexpr
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

resolve_matches_base = function(x) {
  if (nrow(x) <= 1) {
    return(matrix(x, ncol = 4))
  }

  x = x[order(x[, 3], decreasing = TRUE), , drop = FALSE]
  x = filter_overlap_base(x)
  x[order(x[, 2]), , drop = FALSE]
}
