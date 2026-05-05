#mgsub--------
context("Primary Call")

test_that("basic functionality", {
  expect_equal(mgsub("hey, ho", pattern = c("hey", "ho"), replacement = c("ho", "hey")), "ho, hey")
  expect_equal(mgsub("Production Workers, All Other", c("s$", "s([[:punct:] ])", ", ? All Other$"),
                     c("", "\\1", "")), "Production Workers")
})

test_that("non-named mgsub and named sub works", {
  expect_equal(mgsub("Dopazamine and dopaloramide are fake chemicals.",
                     c("dopa", "fake"), c("meta", "real"), ignore.case = TRUE),
               "metazamine and metaloramide are real chemicals.")
  expect_equal(mgsub("Hey, ho", ignore.case = TRUE, c("hey"), c("tomorrow")), "tomorrow, ho")
})

test_that("partially named mgsub inputs works", {
  expect_equal(mgsub("Dopazamine and dopaloramide are fake chemicals.",
                     pattern = c("dopa", "fake"), c("meta", "real"), ignore.case = TRUE),
               "metazamine and metaloramide are real chemicals.")
  expect_equal(mgsub("Dopazamine and dopaloramide are fake chemicals.",
                     c("meta", "real"), pattern = c("dopa", "fake"), ignore.case = TRUE),
               "metazamine and metaloramide are real chemicals.")
})

test_that("multiple input strings are processed", {
  expect_equal(mgsub(c("string", "test"), c("t"), c("p")), c("spring", "pesp"))
  expect_equal(mgsub(c("Dopazamine is not the same as Dopachloride and is still fake.",
                       "dopazamine is undergoing a review by the fake news arm of the Dopazamine government"),
                     c("[Dd]opa(.*?mine)", "fake"), c("Meta\\1", "real")),
                     c("Metazamine is not the same as Dopachloride and is still real.",
                       "Metazamine is undergoing a review by the real news arm of the Metazamine government"))
})

test_that("NAs are correctly handled", {
  expect_equal(mgsub(c("string", NA, "test"), c("t"), c("p")), c("spring", NA, "pesp"))
})

test_that("numeric input is handled gracefully", {
  expect_equal(mgsub("I live in the 37664 zip code but I'm moving to 99516", 
                     pattern = 37664, replacement=37660),
               "I live in the 37660 zip code but I'm moving to 99516")
  expect_equal(mgsub("I live in the 37664 zip code but I'm moving to 99516", 
                     pattern = c(37664, "99516"), replacement=c("37660", 58126)),
               "I live in the 37660 zip code but I'm moving to 58126")
  expect_equal(mgsub(123414, 4, "a"), "123a1a")
})

test_that("recylce has to be a boolean", {
  expect_error(mgsub("hey, ho", c("hey"), c("ho", "hey"), recycle = "yes"))
  expect_error(mgsub("hey, ho", c("hey"), c("ho", "hey"), recycle = 1))
})

test_that("non-recycled non-equal length match and replace input fails", {
  expect_error(mgsub("hey, ho", c("hey", "ho"), c("yo")))
  expect_error(mgsub("hey, ho", c("hey"), c("ho", "yo")))
})

test_that("recycled longer replace than match warns and truncates", {
  expect_warning(mgsub("hey, ho", c("hey"), c("ho", "hey"), recycle = TRUE))
})

test_that("recycled replacements works", {
  expect_equal(mgsub("hey, ho", c("hey", "ho"), "yo", recycle = TRUE), "yo, yo")
})

test_that("works even with start/end symbols", {
  expect_equal(mgsub(c("hi there", "who said hi to me?", "who said hi"),
                     c("^hi"), c("bye")), c("bye there", "who said hi to me?", "who said hi"))
  expect_equal(mgsub(c("hi there", "who said hi to me?", "who said hi"),
                     c("hi$"), c("bye")), c("hi there", "who said hi to me?", "who said bye"))
})

test_that("all NA fails quickly", {
  expect_equal(mgsub(rep(NA, 4), "A", "b"), rep(NA, 4))
  expect_equal(mgsub(NA, "A", "b"), NA)
})

#Worker --------
context("Worker")

test_that("Letter substitution works", {
  expect_equal(unlist(worker("ho ho hoot", c("h", "o"), c("o", "h"))), "oh oh ohht")
  expect_equal(unlist(worker("developer", c("e", "p"), c("p", "e"))), "dpvploepr")
})

test_that("Non-equilength matches replace", {
  expect_equal(unlist(worker("hey, ho", c("hey", "ho"), c("ho", "hey"))), "ho, hey")
  expect_equal(unlist(worker("hi there, buddy boy!", c("there", "buddy"), c("where", "enemy"))), "hi where, enemy boy!")
})

test_that("Substring matches are superceded by longer matches", {
  expect_equal(unlist(worker("they don't understand the value of what they seek.",
                             c("the", "they"), c("a", "we"))),
               "we don't understand a value of what we seek.")
})

test_that("Same length matches are safely converted", {
  expect_equal(unlist(worker("hey, how are you?", c("hey", "how", "are", "you"),
                             c("how", "are", "you", "hey"))), "how, are you hey?")
})

test_that("Regular expression matches work", {
  expect_equal(unlist(worker("Dopazamine (of dopazamines family) is not the same as Dopachloride and is still fake.",
                             c("[Dd]opa", "fake"), c("Meta", "real"))),
               "Metazamine (of Metazamines family) is not the same as Metachloride and is still real.")
})

test_that("Regular expression substitions work", {
  expect_equal(unlist(worker("Dopazamine is not the same as Dopachloride and is still fake.",
                             c("[Dd]opa(.*?mine)", "fake"), c("Meta\\1", "real"))),
               "Metazamine is not the same as Dopachloride and is still real.")
})

test_that("Options passed to sub family work", {
  expect_equal(unlist(worker("Dopazamine and dopaloramide are fake chemicals.",
                             c("dopa", "fake"), c("meta", "real"), ignore.case = TRUE)),
               "metazamine and metaloramide are real chemicals.")
  expect_equal(unlist(worker("Where are the \\bspaces\\b - not the spaces I want?",
                             c("\\bspaces\\b", "[Ww]here"), c("boxes", "There"),
                             fixed = TRUE)), "Where are the boxes - not the spaces I want?")
})

test_that("Priority is based on matched length", {
  expect_equal(worker("Dopazamine is a fake chemical", c("do.*ne", "dopazamin"),
                      c("metazamine", "freakout"), ignore.case = TRUE),
               "metazamine is a fake chemical")
})

test_that("all missing patterns works", {
  expect_equal(worker("hi there", c("why", "not", "go"), c("a", "b", "c")), "hi there")
})

test_that("some missing patterns work", {
  expect_equal(worker("hi there", c("hi", "bye"), c("bye", "hi")), "bye there")
})

test_that("two patterns, only overlap, fast exit", {
  expect_equal(worker("the the the", c("the", "th"), c("a", "b")), "a a a")
})

test_that("get_matches preserves matrix shape and content", {
  x = get_matches("alpha beta alpha", c("alpha", "beta"), 1, fixed = TRUE)
  expect_equal(x,
               matrix(c(1, 1, 5, 5,
                        1, 12, 5, 16),
                      byrow = TRUE, ncol = 4))
  expect_equal(dim(x), c(2, 4))
})

test_that("get_matches_base preserves matrix shape and content", {
  x = get_matches_base("alpha beta alpha", c("alpha", "beta"), 1, fixed = TRUE)
  expect_equal(x,
               matrix(c(1, 1, 5, 5,
                        1, 12, 5, 16),
                      byrow = TRUE, ncol = 4))
  expect_equal(dim(x), c(2, 4))
})

test_that("get_matches falls back to base implementation when native code is unavailable", {
  mgsub_runtime$native_fallback_warned = FALSE

  local_mocked_bindings(
    has_get_matches_native = function() FALSE,
    .package = "mgsub"
  )

  expect_warning(
    result <- get_matches("alpha beta alpha", c("alpha", "beta"), 1, fixed = TRUE),
    "Using the base R fallback"
  )
  expect_equal(result,
               get_matches_base("alpha beta alpha", c("alpha", "beta"), 1,
                                fixed = TRUE))
})

test_that("native get_matches validates dots and index inputs", {
  expect_error(
    .Call("_mgsub_get_matches_cpp", "alpha beta alpha", c("alpha", "beta"),
          1L, 1L, PACKAGE = "mgsub"),
    "dots must be a list"
  )

  expect_error(
    .Call("_mgsub_get_matches_cpp", "alpha beta alpha", c("alpha", "beta"),
          c(1L, 2L), list(fixed = TRUE), PACKAGE = "mgsub"),
    "i must have length 1"
  )

  expect_error(
    .Call("_mgsub_get_matches_cpp", "alpha beta alpha", c("alpha", "beta"),
          NA_integer_, list(fixed = TRUE), PACKAGE = "mgsub"),
    "i must not be NA"
  )

  expect_error(
    .Call("_mgsub_get_matches_cpp", "alpha beta alpha", c("alpha", "beta"),
          NA_real_, list(fixed = TRUE), PACKAGE = "mgsub"),
    "i must not be NA"
  )

  expect_error(
    .Call("_mgsub_get_matches_cpp", "alpha beta alpha", c("alpha", "beta"),
          "1", list(fixed = TRUE), PACKAGE = "mgsub"),
    "i must be numeric"
  )
})

test_that("native get_matches accepts integer indices and validates pattern bounds", {
  x = .Call("_mgsub_get_matches_cpp", "alpha beta alpha", c("alpha", "beta"),
            1L, list(fixed = TRUE), PACKAGE = "mgsub")
  expect_equal(x,
               matrix(c(1, 1, 5, 5,
                        1, 12, 5, 16),
                      byrow = TRUE, ncol = 4))

  expect_error(
    .Call("_mgsub_get_matches_cpp", "alpha beta alpha", 1,
          1L, list(fixed = TRUE), PACKAGE = "mgsub"),
    "pattern must be a character vector"
  )

  expect_error(
    .Call("_mgsub_get_matches_cpp", "alpha beta alpha", c("alpha", "beta"),
          3L, list(fixed = TRUE), PACKAGE = "mgsub"),
    "i is out of bounds"
  )
})

test_that("native get_matches handles malformed gregexpr output", {
  local_mocked_bindings(
    gregexpr = function(...) 1L,
    .package = "base"
  )

  expect_error(
    .Call("_mgsub_get_matches_cpp", "alpha beta alpha", c("alpha", "beta"),
          1L, list(fixed = TRUE), PACKAGE = "mgsub"),
    "gregexpr did not return a list"
  )
})

test_that("native get_matches requires match.length attribute", {
  local_mocked_bindings(
    gregexpr = function(...) list(1L),
    .package = "base"
  )

  expect_error(
    .Call("_mgsub_get_matches_cpp", "alpha beta alpha", c("alpha", "beta"),
          1L, list(fixed = TRUE), PACKAGE = "mgsub"),
    "gregexpr result did not include match.length"
  )
})

test_that("collect_matches preserves matrix shape and content", {
  x = collect_matches("alpha beta alpha", c("alpha", "beta"), fixed = TRUE)
  expect_equal(x,
               matrix(c(1, 1, 5, 5,
                        1, 12, 5, 16,
                        2, 7, 4, 10),
                      byrow = TRUE, ncol = 4))
  expect_equal(dim(x), c(3, 4))
})

test_that("collect_matches_base preserves matrix shape and content", {
  x = collect_matches_base("alpha beta alpha", c("alpha", "beta"), fixed = TRUE)
  expect_equal(x,
               matrix(c(1, 1, 5, 5,
                        1, 12, 5, 16,
                        2, 7, 4, 10),
                      byrow = TRUE, ncol = 4))
  expect_equal(dim(x), c(3, 4))
})

test_that("collect_matches falls back to base implementation when native code is unavailable", {
  mgsub_runtime$native_fallback_warned = FALSE

  local_mocked_bindings(
    has_collect_matches_native = function() FALSE,
    .package = "mgsub"
  )

  expect_warning(
    result <- collect_matches("alpha beta alpha", c("alpha", "beta"), fixed = TRUE),
    "Using the base R fallback"
  )
  expect_equal(result,
               collect_matches_base("alpha beta alpha", c("alpha", "beta"),
                                    fixed = TRUE))
})

test_that("collect_matches with zero patterns matches existing error behavior", {
  expect_error(collect_matches("alpha beta alpha", character(0), fixed = TRUE))
})

test_that("native collect_matches validates pattern input", {
  expect_error(
    .Call("_mgsub_collect_matches_cpp", "alpha beta alpha", 1,
          list(fixed = TRUE), PACKAGE = "mgsub"),
    "pattern must be a character vector"
  )
})

test_that("native collect_matches handles malformed gregexpr output", {
  local_mocked_bindings(
    gregexpr = function(...) 1L,
    .package = "base"
  )

  expect_error(
    .Call("_mgsub_collect_matches_cpp", "alpha beta alpha", c("alpha", "beta"),
          list(fixed = TRUE), PACKAGE = "mgsub"),
    "gregexpr did not return a list"
  )
})

test_that("native collect_matches requires match.length attribute", {
  local_mocked_bindings(
    gregexpr = function(...) list(1L),
    .package = "base"
  )

  expect_error(
    .Call("_mgsub_collect_matches_cpp", "alpha beta alpha", c("alpha", "beta"),
          list(fixed = TRUE), PACKAGE = "mgsub"),
    "gregexpr result did not include match.length"
  )
})

test_that("filter_overlap preserves overlap precedence and matrix shape", {
  x = matrix(c(1, 1, 4, 4,
               2, 1, 3, 3,
               3, 6, 2, 7),
             byrow = TRUE, ncol = 4)
  expect_equal(filter_overlap(x),
               matrix(c(1, 1, 4, 4,
                        3, 6, 2, 7),
                      byrow = TRUE, ncol = 4))

  one = filter_overlap(matrix(c(1, 2, 3, 4), ncol = 4))
  expect_equal(dim(one), c(1, 4))
})

test_that("native filter_overlap handles integer, numeric, and empty matrices", {
  x_int = matrix(c(1L, 1L, 4L, 4L,
                   2L, 1L, 3L, 3L,
                   3L, 6L, 2L, 7L),
                 byrow = TRUE, ncol = 4)
  expect_equal(
    .Call("_mgsub_filter_overlap_cpp", x_int, PACKAGE = "mgsub"),
    matrix(c(1L, 1L, 4L, 4L,
             3L, 6L, 2L, 7L),
           byrow = TRUE, ncol = 4)
  )

  x_num = matrix(c(1, 1, 4, 4,
                   2, 1, 3, 3,
                   3, 6, 2, 7),
                 byrow = TRUE, ncol = 4)
  expect_equal(
    .Call("_mgsub_filter_overlap_cpp", x_num, PACKAGE = "mgsub"),
    matrix(c(1, 1, 4, 4,
             3, 6, 2, 7),
           byrow = TRUE, ncol = 4)
  )

  empty = matrix(integer(0), ncol = 4)
  result = .Call("_mgsub_filter_overlap_cpp", empty, PACKAGE = "mgsub")
  expect_equal(dim(result), c(0, 4))
  expect_type(result, "integer")
})

test_that("native filter_overlap validates matrix input", {
  expect_error(
    .Call("_mgsub_filter_overlap_cpp", c(1L, 1L, 4L, 4L), PACKAGE = "mgsub"),
    "x must be a matrix with 4 columns"
  )

  expect_error(
    .Call("_mgsub_filter_overlap_cpp", matrix(1L, ncol = 3), PACKAGE = "mgsub"),
    "x must be a matrix with 4 columns"
  )

  expect_error(
    .Call("_mgsub_filter_overlap_cpp",
          matrix(TRUE, nrow = 1, ncol = 4), PACKAGE = "mgsub"),
    "x must be an integer or numeric matrix"
  )
})

test_that("native filter_overlap skips previously discarded rows correctly", {
  x = matrix(c(1L, 1L, 5L, 5L,
               2L, 1L, 4L, 4L,
               3L, 1L, 3L, 3L,
               4L, 8L, 2L, 9L),
             byrow = TRUE, ncol = 4)

  expect_equal(
    .Call("_mgsub_filter_overlap_cpp", x, PACKAGE = "mgsub"),
    matrix(c(1L, 1L, 5L, 5L,
             4L, 8L, 2L, 9L),
           byrow = TRUE, ncol = 4)
  )
})

test_that("filter_overlap_base preserves overlap precedence and matrix shape", {
  x = matrix(c(1, 1, 4, 4,
               2, 1, 3, 3,
               3, 6, 2, 7),
             byrow = TRUE, ncol = 4)
  expect_equal(filter_overlap_base(x),
               matrix(c(1, 1, 4, 4,
                        3, 6, 2, 7),
                      byrow = TRUE, ncol = 4))
})

test_that("filter_overlap_base drops rows when only the end position overlaps", {
  x = matrix(c(1, 3, 3, 5,
               2, 1, 4, 4),
             byrow = TRUE, ncol = 4)

  expect_equal(
    filter_overlap_base(x),
    matrix(c(1, 3, 3, 5), ncol = 4)
  )
})

test_that("resolve_matches preserves ordering and overlap resolution", {
  x = matrix(c(1, 1, 4, 4,
               2, 1, 3, 3,
               3, 6, 2, 7),
             byrow = TRUE, ncol = 4)
  expect_equal(resolve_matches(x),
               matrix(c(1, 1, 4, 4,
                        3, 6, 2, 7),
                      byrow = TRUE, ncol = 4))
})

test_that("native resolve_matches handles integer, numeric, and short matrices", {
  x_int = matrix(c(1L, 1L, 4L, 4L,
                   2L, 1L, 3L, 3L,
                   3L, 6L, 2L, 7L),
                 byrow = TRUE, ncol = 4)
  expect_equal(
    .Call("_mgsub_resolve_matches_cpp", x_int, PACKAGE = "mgsub"),
    matrix(c(1L, 1L, 4L, 4L,
             3L, 6L, 2L, 7L),
           byrow = TRUE, ncol = 4)
  )

  x_num = matrix(c(1, 1, 4, 4,
                   2, 1, 3, 3,
                   3, 6, 2, 7),
                 byrow = TRUE, ncol = 4)
  expect_equal(
    .Call("_mgsub_resolve_matches_cpp", x_num, PACKAGE = "mgsub"),
    matrix(c(1, 1, 4, 4,
             3, 6, 2, 7),
           byrow = TRUE, ncol = 4)
  )

  one = matrix(c(1L, 2L, 3L, 4L), ncol = 4)
  expect_equal(.Call("_mgsub_resolve_matches_cpp", one, PACKAGE = "mgsub"), one)

  empty = matrix(integer(0), ncol = 4)
  result = .Call("_mgsub_resolve_matches_cpp", empty, PACKAGE = "mgsub")
  expect_equal(dim(result), c(0, 4))
  expect_type(result, "integer")
})

test_that("native resolve_matches validates matrix input", {
  expect_error(
    .Call("_mgsub_resolve_matches_cpp", c(1L, 1L, 4L, 4L), PACKAGE = "mgsub"),
    "x must be a matrix with 4 columns"
  )

  expect_error(
    .Call("_mgsub_resolve_matches_cpp", matrix(1L, ncol = 3), PACKAGE = "mgsub"),
    "x must be a matrix with 4 columns"
  )

  expect_error(
    .Call("_mgsub_resolve_matches_cpp",
          matrix(TRUE, nrow = 1, ncol = 4), PACKAGE = "mgsub"),
    "x must be an integer or numeric matrix"
  )
})

test_that("resolve_matches_base preserves ordering and overlap resolution", {
  x = matrix(c(1, 1, 4, 4,
               2, 1, 3, 3,
               3, 6, 2, 7),
             byrow = TRUE, ncol = 4)
  expect_equal(resolve_matches_base(x),
               matrix(c(1, 1, 4, 4,
                        3, 6, 2, 7),
                      byrow = TRUE, ncol = 4))

  one = resolve_matches_base(matrix(c(1, 2, 3, 4), ncol = 4))
  expect_equal(dim(one), c(1, 4))
})

test_that("resolve_matches falls back to base implementation when native code is unavailable", {
  x = matrix(c(1, 1, 4, 4,
               2, 1, 3, 3,
               3, 6, 2, 7),
             byrow = TRUE, ncol = 4)

  mgsub_runtime$native_fallback_warned = FALSE

  local_mocked_bindings(
    has_resolve_matches_native = function() FALSE,
    .package = "mgsub"
  )

  expect_warning(
    result <- resolve_matches(x),
    "Using the base R fallback"
  )
  expect_equal(result, resolve_matches_base(x))
})

test_that("filter_overlap falls back to base implementation when native code is unavailable", {
  x = matrix(c(1, 1, 4, 4,
               2, 1, 3, 3,
               3, 6, 2, 7),
             byrow = TRUE, ncol = 4)

  mgsub_runtime$native_fallback_warned = FALSE

  local_mocked_bindings(
    has_filter_overlap_native = function() FALSE,
    .package = "mgsub"
  )

  expect_warning(
    result <- filter_overlap(x),
    "Using the base R fallback"
  )
  expect_equal(result, filter_overlap_base(x))
})

test_that("native fallback warning is emitted only once across helpers", {
  x = matrix(c(1, 1, 4, 4,
               2, 1, 3, 3,
               3, 6, 2, 7),
             byrow = TRUE, ncol = 4)

  mgsub_runtime$native_fallback_warned = FALSE

  local_mocked_bindings(
    has_get_matches_native = function() FALSE,
    has_filter_overlap_native = function() FALSE,
    .package = "mgsub"
  )

  expect_warning(
    get_matches("alpha beta alpha", c("alpha", "beta"), 1, fixed = TRUE),
    "Using the base R fallback"
  )
  expect_no_warning(filter_overlap(x))
})

test_that("native fallback warning can be suppressed by option", {
  x = matrix(c(1, 1, 4, 4,
               2, 1, 3, 3,
               3, 6, 2, 7),
             byrow = TRUE, ncol = 4)

  mgsub_runtime$native_fallback_warned = FALSE
  old = options(mgsub.warn_native_fallback = FALSE)
  on.exit(options(old), add = TRUE)

  local_mocked_bindings(
    has_filter_overlap_native = function() FALSE,
    .package = "mgsub"
  )

  expect_no_warning(result <- filter_overlap(x))
  expect_equal(result, filter_overlap_base(x))
  expect_false(mgsub_runtime$native_fallback_warned)
})
