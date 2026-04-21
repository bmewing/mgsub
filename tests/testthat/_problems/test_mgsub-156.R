# Extracted from test_mgsub.R:156

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
expect_equal(filter_overlap_base(x),
               matrix(c(1, 1, 4, 4,
                        3, 6, 2, 7),
                      byrow = TRUE, ncol = 4))
one = filter_overlap_base(matrix(c(1, 2, 3, 4), ncol = 4))
