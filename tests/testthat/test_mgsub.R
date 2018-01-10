context("Substitution")

test_that("Letter substitution works",{
  expect_equal(mgsub("ho ho hoot",list("h"="o","o"="h")),"oh oh ohht")
  expect_equal(mgsub("developer",list("e" ="p", "p" = "e")),"dpvploepr")
})

test_that("Non-equilength matches replace",{
  expect_equal(mgsub("hey, ho",list("hey"="ho","ho"="hey")),"ho, hey")
  expect_equal(mgsub("hi there, buddy boy!",list("there"="where","buddy"="enemy")),"hi where, enemy boy!")
})

test_that("Substring matches are superceded by longer matches",{
  expect_equal(mgsub("they don't understand the value of what they seek.",list("the"="a","they"="we")),"we don't understand a value of what we seek.")
})

test_that("Same length matches are safely converted",{
  expect_equal(mgsub("hey, how are you?",list("hey"="how","how"="are","are"="you","you"="hey")),"how, are you hey?")
})

test_that("Regular expression matches work",{
  expect_equal(mgsub("Dopazamine (of dopazamines family) is not the same as Dopachloride and is still fake.",list("[Dd]opa"="Meta","fake"="real")),"Metazamine (of Metazamines family) is not the same as Metachloride and is still real.")
})

test_that("Regular expression substitions work",{
  expect_equal(mgsub("Dopazamine is not the same as Dopachloride and is still fake.",list("[Dd]opa(.*?mine)"="Meta\\1","fake"="real")),"Metazamine is not the same as Dopachloride and is still real.")
})

test_that("Options passed to sub family work",{
  expect_equal(mgsub("Dopazamine and dopaloramide are fake chemicals.",list("dopa"="meta","fake"="real"),ignore.case=TRUE),"metazamine and metaloramide are real chemicals.")
  expect_equal(mgsub("Where are the \\bspaces\\b - not the spaces I want?",list("\\bspaces\\b"="boxes","[Ww]here"="There"),fixed=TRUE),"Where are the boxes - not the spaces I want?")
})
