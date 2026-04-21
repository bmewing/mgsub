# Extracted from test_mgsub.R:173

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "mgsub", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
context("Primary Call")
context("Worker")

# test -------------------------------------------------------------------------
x = matrix(c(1, 1, 4, 4,
               2, 1, 3, 3,
               3, 6, 2, 7),
             byrow = TRUE, ncol = 4)
mgsub_runtime$filter_overlap_warned = FALSE
local_mocked_bindings(
    has_filter_overlap_native = function() FALSE,
    .package = "mgsub"
  )
expect_warning(
    result = filter_overlap(x),
    "Using the base R fallback for filter_overlap"
  )
